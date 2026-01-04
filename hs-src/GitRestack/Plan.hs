{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitRestack.Plan
  ( PlanOptions(..)
  , runPlan
  ) where

import Control.Exception (SomeException, catch, finally, throwIO)
import Control.Monad (forM_, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import GitRestack.Conflict (finishCherryPick)
import GitRestack.Diff
import GitRestack.Git
import GitRestack.Stack
import GitRestack.Types
import GitRestack.Utils
import GitRestack.Yaml
import System.Directory
import System.Exit (exitWith)
import System.FilePath ((</>), takeFileName)
import System.IO (hFlush, hGetLine, hIsEOF, hIsTerminalDevice, stdin, stdout)


data PlanOptions = PlanOptions
  { poOutputFile :: FilePath
  , poVerifyCmd :: Maybe Text
  , poVerifyOnly :: Bool
  , poStagedOnly :: Bool
  , poUnstagedOnly :: Bool
  , poMode :: PlanMode
  , poWorktreePath :: Maybe FilePath
  , poMergetool :: Maybe Text
  , poForce :: Bool
  }

runPlan :: PlanOptions -> IO ()
runPlan opts = do
  putStrLn "Analyzing branch stack..."
  stack <- analyzeStack `catch` \(_ :: SomeException) -> do
    putStrLn "Error: Could not analyze stack. Are you in a git repository?"
    exitWith (ExitFailure 1)

  putStrLn ("Found " <> show (length (stBranches stack)) <> " branches in stack")

  files <- if poVerifyOnly opts
    then do
      putStrLn "Verify-only mode: skipping change collection"
      pure []
    else collectFiles opts

  (branchesWithFixes, errors) <- mapFilesToBranches stack files

  (simulation, conflicts) <- if null (stBranches stack)
    then pure (Nothing, [])
    else do
      outcome <- simulateConflicts stack branchesWithFixes (poMode opts) (poWorktreePath opts) (poMergetool opts) (poForce opts)
        `catch` \(GitError msg) -> do
          if "worktree" `Text.isInfixOf` Text.pack msg
            then putStrLn "Error: Plan worktree already exists. Use --force or cleanup first."
            else putStrLn ("Error: " <> msg)
          exitWith (ExitFailure 1)
      pure (Just (simSimulation outcome), simConflicts outcome)

  cwd <- getCwd
  timestamp <- formatTimestamp

  let plan = Plan
        { planVersion = 2
        , planGenerated = timestamp
        , planRepository = Text.pack cwd
        , planVerifyCmd = poVerifyCmd opts
        , planSimulation = simulation
        , planErrors = errors
        , planConflicts = conflicts
        , planStack = stack
            { stBranches = branchesWithFixes
            }
        }

  createDirectoryIfMissing True ".git/git-restack"
  BS.writeFile (poOutputFile opts) (emitPlan plan)

  putStrLn ("\nPlan written to: " <> poOutputFile opts)

  when (not (null conflicts)) $
    putStrLn ("\nResolved: " <> show (length conflicts) <> " conflicts captured in plan.")

  if not (null errors)
    then do
      putStrLn ("\nWarning: Plan has " <> show (length errors) <> " unresolved errors.\n")
      forM_ (zip [1 :: Int ..] errors) $ \(idx, err) -> do
        putStrLn ("  " <> show idx <> ". [" <> Text.unpack (errorTypeToText (peType err)) <> "] " <> pePath err)
        putStrLn ("     " <> Text.unpack (peMessage err) <> "\n")
      putStrLn ("Fix these in " <> poOutputFile opts <> ", then run: git-restack exec " <> poOutputFile opts)
      exitWith (ExitFailure 1)
    else do
      let fixCount = length (filter sbNeedsFix branchesWithFixes)
      if fixCount == 0
        then putStrLn "\nNo changes to apply. All files already in correct branches."
        else do
          putStrLn ("\nSuccess: " <> show fixCount <> " branches will receive fixes.")
          putStrLn ("Review the plan and run: git-restack exec " <> poOutputFile opts)

collectFiles :: PlanOptions -> IO [ChangedFile]
collectFiles opts = do
  putStrLn "Collecting changed files..."
  staged <- if poUnstagedOnly opts
    then pure []
    else getStagedFiles
  unstaged <- if poStagedOnly opts
    then pure []
    else getUnstagedFiles
  let combined = staged ++ filter (\file -> changedPath file `notElem` map changedPath staged) unstaged
  putStrLn ("Found " <> show (length combined) <> " changed files")
  pure combined

mapFilesToBranches :: Stack -> [ChangedFile] -> IO ([StackBranch], [PlanError])
mapFilesToBranches stack files = do
  let branches = map initBranch (stBranches stack)
  (updated, errors) <- foldl processFile (pure (branches, [])) files
  pure (updated, errors)
  where
    initBranch branch = branch
      { sbNeedsFix = False
      , sbFix = Nothing
      , sbStepStatus = StepPending
      }

    processFile action file = do
      (branchesAcc, errs) <- action
      putStrLn ("  Mapping: " <> changedPath file)
      lastCommit <- getLastCommitForFile (changedPath file)
      case lastCommit of
        Nothing -> do
          let err = PlanError
                { peType = ErrorUnmappedFile
                , pePath = changedPath file
                , peMessage = "New file detected. Assign to a branch fix block manually."
                }
          pure (branchesAcc, errs ++ [err])
        Just commit -> do
          inAncestry <- isCommitInAncestry commit (stBaseCommit stack)
          let isBase = commit == stBaseCommit stack
          if not inAncestry && not isBase
            then do
              let err = PlanError
                    { peType = ErrorOutsideAncestry
                    , pePath = changedPath file
                    , peMessage = "File modified from commit outside stack ancestry."
                    }
              pure (branchesAcc, errs ++ [err])
            else do
              targetBranch <- findBranchForCommit commit stack
              case targetBranch of
                Nothing -> do
                  let err = PlanError
                        { peType = ErrorAmbiguousBranch
                        , pePath = changedPath file
                        , peMessage = "Could not determine target branch for this file."
                        }
                  pure (branchesAcc, errs ++ [err])
                Just branchName -> do
                  diffContent <- getFileDiff (changedPath file) (changedStaged file)
                  let fileChange = FileChange
                        { fcPath = changedPath file
                        , fcChangeType = changedType file
                        , fcDiff = diffContent
                        , fcStaged = changedStaged file
                        }
                  let updatedBranches = map (applyFix branchName fileChange) branchesAcc
                  pure (updatedBranches, errs)

    applyFix branchName fileChange branch
      | sbName branch /= branchName = branch
      | otherwise =
          let jiraPrefix = fromMaybe "FIX" (extractJiraPrefix (sbName branch))
              commitMessage = "fix: Apply changes\n\nJira: " <> jiraPrefix
              currentFix = fromMaybe (Fix commitMessage []) (sbFix branch)
              updatedFix = currentFix { fixFiles = fixFiles currentFix ++ [fileChange] }
          in branch
              { sbNeedsFix = True
              , sbFix = Just updatedFix
              }

data SimulationOutcome = SimulationOutcome
  { simSimulation :: PlanSimulation
  , simConflicts :: [PlanConflict]
  }

simulateConflicts :: Stack -> [StackBranch] -> PlanMode -> Maybe FilePath -> Maybe Text -> Bool -> IO SimulationOutcome
simulateConflicts stack branches mode worktreePath mergetool forceFlag = do
  timestamp <- currentMillis
  let timestampStr = show timestamp
  let planBranch = "git-restack-plan-" <> timestampStr
  cwd <- getCwd

  let defaultPlanPath = "../" <> takeFileName cwd <> "-restack-plan"
  let actualWorktreePath = fromMaybe defaultPlanPath worktreePath

  let initialSimulation = PlanSimulation
        { simMode = mode
        , simWorktreePath = if mode == PlanModeWorktree then Just actualWorktreePath else Nothing
        , simPlanBranch = Just (Text.pack planBranch)
        , simBackupBranches = []
        }

  (simPath, createdWorktree, createdPlanBranch, originalBranch, stashed, backups) <-
    setupSimulation stack branches mode actualWorktreePath planBranch timestampStr forceFlag

  let simulation = initialSimulation { simBackupBranches = backups }

  conflicts <- finally
    (simulateStack simPath branches mergetool)
    (cleanupSimulation simPath mode (Text.pack planBranch) createdWorktree createdPlanBranch originalBranch stashed (stBaseBranch stack))

  pure SimulationOutcome
    { simSimulation = simulation
    , simConflicts = conflicts
    }

setupSimulation :: Stack -> [StackBranch] -> PlanMode -> FilePath -> String -> String -> Bool -> IO (FilePath, Bool, Bool, Maybe Text, Bool, [BackupBranch])
setupSimulation stack branches mode worktreePath planBranch timestampStr forceFlag = do
  cwd <- getCwd
  case mode of
    PlanModeWorktree -> do
      exists <- doesPathExist worktreePath
      when (exists && not forceFlag) $ throwIO (GitError "Plan worktree exists")
      when exists $ do
        _ <- runGitWithStatus ["worktree", "remove", worktreePath, "--force"]
        pure ()
      _ <- runGit ["worktree", "add", worktreePath, Text.unpack (stBaseBranch stack)]
      _ <- runGitWithStatus ["-C", worktreePath, "checkout", "-b", planBranch]
      pure (worktreePath, True, True, Nothing, False, [])
    PlanModeDirect -> do
      original <- getCurrentBranch
      stashed <- stashIfDirty
      backups <- mapM (backupBranch timestampStr) branches
      _ <- runGit ["checkout", Text.unpack (stBaseBranch stack)]
      _ <- runGitWithStatus ["checkout", "-b", planBranch]
      pure (cwd, False, True, Just original, stashed, backups)

backupBranch :: String -> StackBranch -> IO BackupBranch
backupBranch timestampStr branch = do
  let backupName = Text.unpack (sbName branch) <> "-restack-backup-" <> timestampStr
  _ <- runGit ["branch", backupName, Text.unpack (sbName branch)]
  pure BackupBranch
    { backupSource = sbName branch
    , backupName = Text.pack backupName
    }

getCurrentBranch :: IO Text
getCurrentBranch = do
  out <- runGit ["symbolic-ref", "--short", "HEAD"]
  pure (TextEnc.decodeUtf8 (trimBS out))

stashIfDirty :: IO Bool
stashIfDirty = do
  status <- runGitWithStatus ["status", "--porcelain"]
  if BSC.null (grStdout status)
    then pure False
    else do
      _ <- runGit ["stash", "push", "-u", "-m", "git-restack-plan"]
      pure True

simulateStack :: FilePath -> [StackBranch] -> Maybe Text -> IO [PlanConflict]
simulateStack simPath branches mergetool = do
  conflicts <- foldl step (pure []) branches
  pure conflicts
  where
    step action branch = do
      acc <- action
      newConflicts <- replayBranch simPath branch mergetool
      pure (acc ++ newConflicts)

replayBranch :: FilePath -> StackBranch -> Maybe Text -> IO [PlanConflict]
replayBranch simPath branch mergetool = do
  cherryConflicts <- replayCommits
  fixConflicts <- if sbNeedsFix branch then applyFixes else pure []
  pure (cherryConflicts ++ fixConflicts)
  where
    replayCommits = case sbParentBranch branch of
      Nothing -> pure []
      Just parent -> do
        let range = Text.unpack (parent <> ".." <> sbName branch)
        commitsOutput <- runGit ["-C", simPath, "rev-list", "--reverse", range]
        let commits = filter (not . Text.null) (map TextEnc.decodeUtf8 (BSC.lines (trimBS commitsOutput)))
        foldl (cherryPickCommit parent) (pure []) commits

    cherryPickCommit _ action commitSha = do
      acc <- action
      result <- runGitWithStatusNoRerere ["-C", simPath, "cherry-pick", Text.unpack commitSha]
      if grExitCode result == 0
        then pure acc
        else do
          subject <- getCommitSubject simPath commitSha
          conflict <- resolveConflict simPath ConflictCherryPick (sbName branch) (Just commitSha) subject mergetool
          continueOk <- finishCherryPick simPath
          unless continueOk $ throwIO (GitError "Cherry-pick --continue failed")
          pure (acc ++ [conflict])

    applyFixes = case sbFix branch of
      Nothing -> pure []
      Just fix -> do
        conflicts <- foldl (applyFile fix) (pure []) (fixFiles fix)
        _ <- runGitWithStatus ["-C", simPath, "add", "-A"]
        _ <- runGitWithStatus ["-C", simPath, "commit", "-m", Text.unpack (fixCommitMessage fix), "--allow-empty"]
        pure conflicts

    applyFile _ action file = do
      acc <- action
      let tmpPath = simPath </> ".git-restack-patch"
      BS.writeFile tmpPath (fcDiff file)
      result <- runGitWithStatusNoRerere ["-C", simPath, "apply", "--3way", "--allow-empty", tmpPath]
      if grExitCode result /= 0
        then do
          conflict <- resolveConflict simPath ConflictFixApply (sbName branch) Nothing Nothing mergetool
          removePath tmpPath
          pure (acc ++ [conflict])
        else do
          removePath tmpPath
          pure acc

resolveConflict :: FilePath -> ConflictKind -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO PlanConflict
resolveConflict simPath kind branch commit subject mergetool = do
  putStrLn ""
  putStrLn ("Conflict detected during " <> Text.unpack (conflictKindToText kind) <> ".")
  putStrLn ("Resolve in: " <> simPath)
  case commit of
    Just sha -> putStrLn ("Commit: " <> Text.unpack (Text.take 7 sha))
    Nothing -> pure ()
  putStrLn ("Branch: " <> Text.unpack branch <> "\n")

  conflictPaths <- getConflictedFiles simPath
  when (null conflictPaths) $ throwIO (GitError "Conflict detected but no files found")

  files <- mapM (captureConflictFile simPath) conflictPaths
  _ <- runMergetool simPath mergetool

  isTty <- hIsTerminalDevice stdin
  let loop attemptedAuto = do
        remaining <- getConflictedFiles simPath
        if null remaining
          then pure ()
          else do
            autoHandled <- if attemptedAuto then pure False else autoResolveRemaining simPath remaining mergetool
            if autoHandled
              then loop True
              else do
                putStrLn "Unresolved files:"
                forM_ remaining $ \path -> putStrLn ("  - " <> path)
                putStrLn "\nResolve conflicts, then press Enter to continue (or type 'abort')."
                hFlush stdout
                input <- readLine
                if Text.null input && not isTty
                  then throwIO (GitError "Conflict resolution aborted")
                  else if Text.strip input == "abort"
                    then throwIO (GitError "Conflict resolution aborted")
                    else loop True

  loop False

  resolvedFiles <- mapM (captureResolution simPath) files
  _ <- runGitWithStatus ["-C", simPath, "add", "-A"]

  pure PlanConflict
    { pcKind = kind
    , pcBranch = branch
    , pcCommit = commit
    , pcSubject = subject
    , pcFiles = resolvedFiles
    }

captureConflictFile :: FilePath -> FilePath -> IO ConflictFile
captureConflictFile simPath path = do
  conflictDiff <- getConflictDiff simPath path
  preferGitlink <- isGitlinkConflict simPath path
  let resolution = ConflictResolution
        { resPresent = True
        , resEncoding = if preferGitlink then EncodingGitlink else EncodingText
        , resContent = ""
        }
  pure ConflictFile
    { cfPath = path
    , cfConflictDiff = conflictDiff
    , cfResolution = resolution
    }

captureResolution :: FilePath -> ConflictFile -> IO ConflictFile
captureResolution simPath file = do
  let preferGitlink = resEncoding (cfResolution file) == EncodingGitlink
  resolution <- captureResolutionContent simPath (cfPath file) preferGitlink
  pure file { cfResolution = resolution }

captureResolutionContent :: FilePath -> FilePath -> Bool -> IO ConflictResolution
captureResolutionContent simPath filePath preferGitlink = do
  gitlink <- getGitlinkHash simPath filePath
  case gitlink of
    Just hash -> pure ConflictResolution
      { resPresent = True
      , resEncoding = EncodingGitlink
      , resContent = TextEnc.encodeUtf8 hash
      }
    Nothing | preferGitlink -> pure ConflictResolution
      { resPresent = False
      , resEncoding = EncodingGitlink
      , resContent = ""
      }
    _ -> do
      let fullPath = simPath </> filePath
      exists <- doesPathExist fullPath
      isDir <- doesDirectoryExist fullPath
      if not exists || isDir
        then pure ConflictResolution { resPresent = False, resEncoding = EncodingText, resContent = "" }
        else do
          content <- BS.readFile fullPath
          if isBinary content
            then pure ConflictResolution
              { resPresent = True
              , resEncoding = EncodingBase64
              , resContent = encodeBase64 content
              }
            else pure ConflictResolution
              { resPresent = True
              , resEncoding = EncodingText
              , resContent = content
              }

runMergetool :: FilePath -> Maybe Text -> IO Bool
runMergetool simPath tool = do
  let baseArgs = ["-C", simPath, "mergetool", "--no-prompt"]
  let args = case tool of
        Nothing -> baseArgs
        Just name -> baseArgs ++ ["--tool", Text.unpack name]
  result <- runGitWithStatus args
  pure (grExitCode result == 0)

getConflictDiff :: FilePath -> FilePath -> IO ByteString
getConflictDiff simPath filePath = do
  result <- runGitWithStatus ["-C", simPath, "diff", "--binary", "--", filePath]
  if grExitCode result /= 0
    then pure ""
    else pure (grStdout result)

getCommitSubject :: FilePath -> Text -> IO (Maybe Text)
getCommitSubject simPath commitSha = do
  result <- runGitWithStatus ["-C", simPath, "show", "-s", "--format=%s", Text.unpack commitSha]
  if grExitCode result /= 0 || BSC.null (trimBS (grStdout result))
    then pure Nothing
    else pure (Just (TextEnc.decodeUtf8 (trimBS (grStdout result))))

isGitlinkConflict :: FilePath -> FilePath -> IO Bool
isGitlinkConflict simPath filePath = do
  result <- runGitWithStatus ["-C", simPath, "ls-files", "-u", "--", filePath]
  if grExitCode result /= 0 || BSC.null (trimBS (grStdout result))
    then pure False
    else pure (any hasGitlinkMode (lines (BSC.unpack (trimBS (grStdout result)))))
  where
    hasGitlinkMode line =
      case words line of
        (mode:_) -> mode == "160000"
        _ -> False

getGitlinkHash :: FilePath -> FilePath -> IO (Maybe Text)
getGitlinkHash simPath filePath = do
  result <- runGitWithStatus ["-C", simPath, "ls-files", "--stage", "--", filePath]
  if grExitCode result /= 0 || BSC.null (trimBS (grStdout result))
    then pure Nothing
    else pure (findHash (lines (BSC.unpack (trimBS (grStdout result)))))
  where
    findHash [] = Nothing
    findHash (line:rest) =
      case words line of
        (mode:hash:stage:_) | mode == "160000" && stage == "0" -> Just (Text.pack hash)
        _ -> findHash rest

getGitlinkStageHash :: FilePath -> FilePath -> Text -> IO (Maybe Text)
getGitlinkStageHash simPath filePath stage = do
  result <- runGitWithStatus ["-C", simPath, "ls-files", "-u", "--", filePath]
  if grExitCode result /= 0 || BSC.null (trimBS (grStdout result))
    then pure Nothing
    else pure (findHash (lines (BSC.unpack (trimBS (grStdout result)))))
  where
    findHash [] = Nothing
    findHash (line:rest) =
      case words line of
        (mode:hash:foundStage:_) | mode == "160000" && foundStage == Text.unpack stage -> Just (Text.pack hash)
        _ -> findHash rest

autoResolveRemaining :: FilePath -> [FilePath] -> Maybe Text -> IO Bool
autoResolveRemaining simPath remaining mergetool =
  case detectMergetoolSide mergetool of
    Nothing -> pure False
    Just side -> do
      handledGitlink <- resolveGitlinkConflicts simPath remaining side
      handledDistinct <- resolveDistinctTypeConflicts simPath remaining side
      handled <- resolveSimpleConflicts simPath remaining side
      _ <- runGitWithStatus ["-C", simPath, "add", "-A"]
      pure (handledGitlink || handledDistinct || handled)

resolveGitlinkConflicts :: FilePath -> [FilePath] -> Text -> IO Bool
resolveGitlinkConflicts simPath remaining side = do
  let stage = if side == "theirs" then "3" else "2"
  handled <- mapM (resolveOne stage) remaining
  pure (or handled)
  where
    resolveOne stage path = do
      hash <- getGitlinkStageHash simPath path stage
      case hash of
        Nothing -> pure False
        Just h -> do
          result <- runGitWithStatus ["-C", simPath, "update-index", "--cacheinfo", "160000", Text.unpack h, path]
          pure (grExitCode result == 0)

resolveDistinctTypeConflicts :: FilePath -> [FilePath] -> Text -> IO Bool
resolveDistinctTypeConflicts simPath remaining side = do
  let handledPaths = map conflictBackupBase remaining
  let pairs = [(base, path) | (Just base, path) <- zip handledPaths remaining]
  if null pairs
    then pure False
    else do
      forM_ pairs $ \(baseName, path) -> do
        let basePath = simPath </> baseName
        let headPath = simPath </> path
        if side == "ours"
          then do
            removePath basePath
            exists <- doesPathExist headPath
            when exists $ renamePath headPath basePath
          else removePath headPath
      _ <- runGitWithStatus ["-C", simPath, "add", "-A"]
      pure True

resolveSimpleConflicts :: FilePath -> [FilePath] -> Text -> IO Bool
resolveSimpleConflicts simPath remaining side = do
  handled <- mapM (resolveOne side) remaining
  pure (or handled)
  where
    resolveOne side path = do
      case conflictBackupBase path of
        Just _ -> pure True
        Nothing -> do
          let stage = if side == "theirs" then "3" else "2"
          gitlink <- getGitlinkStageHash simPath path (Text.pack stage)
          case gitlink of
            Just _ -> pure True
            Nothing -> do
              let checkoutArgs = ["-C", simPath, "checkout", if side == "theirs" then "--theirs" else "--ours", "--", path]
              result <- runGitWithStatus checkoutArgs
              if grExitCode result == 0
                then pure True
                else do
                  let rmArgs = ["-C", simPath, "rm", "-f", "-r", "--", path]
                  rmResult <- runGitWithStatus rmArgs
                  pure (grExitCode rmResult == 0)

conflictBackupBase :: FilePath -> Maybe FilePath
conflictBackupBase path =
  case break (== '~') (reverse path) of
    (_, []) -> Nothing
    (suffixRev, _ : restRev) ->
      let suffix = reverse suffixRev
      in if suffix == "HEAD" || (length suffix >= 6 && all isHex (take 6 suffix))
          then Just (reverse restRev)
          else Nothing
  where
    isHex c = c `elem` ("0123456789abcdefABCDEF" :: String)

readLine :: IO Text
readLine = do
  eof <- hIsEOF stdin
  if eof
    then pure ""
    else Text.pack <$> hGetLine stdin

cleanupSimulation :: FilePath -> PlanMode -> Text -> Bool -> Bool -> Maybe Text -> Bool -> Text -> IO ()
cleanupSimulation simPath mode planBranch createdWorktree createdPlanBranch originalBranch stashed baseBranch =
  case mode of
    PlanModeWorktree -> do
      when createdPlanBranch $ do
        _ <- runGitWithStatus ["-C", simPath, "checkout", Text.unpack baseBranch]
        _ <- runGitWithStatus ["branch", "-D", Text.unpack planBranch]
        pure ()
      when createdWorktree $ do
        _ <- runGitWithStatus ["worktree", "remove", simPath, "--force"]
        _ <- runGitWithStatus ["worktree", "prune"]
        pure ()
    PlanModeDirect -> do
      case originalBranch of
        Just branch -> do
          _ <- runGitWithStatus ["checkout", Text.unpack branch]
          pure ()
        Nothing -> pure ()
      when createdPlanBranch $ do
        _ <- runGitWithStatus ["branch", "-D", Text.unpack planBranch]
        pure ()
      when stashed $ do
        _ <- runGitWithStatus ["stash", "pop"]
        pure ()

removePath :: FilePath -> IO ()
removePath path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  if isDir
    then removePathForcibly path
    else if isFile
      then removeFile path
      else pure ()

renamePath :: FilePath -> FilePath -> IO ()
renamePath src dst = do
  isDir <- doesDirectoryExist src
  isFile <- doesFileExist src
  if isDir
    then renameDirectory src dst
    else when isFile (renameFile src dst)
