{-# LANGUAGE OverloadedStrings #-}

module GitRestack.Conflict
  ( applyConflictResolution
  , finishCherryPick
  , findCherryPickConflict
  , findFixApplyConflict
  , pathsMatch
  ) where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (findIndex)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import GitRestack.Diff (getConflictedFiles)
import GitRestack.Git
import GitRestack.Types
import GitRestack.Utils (decodeBase64, trimBS)
import System.Directory
import System.FilePath

applyConflictResolution :: FilePath -> PlanConflict -> IO ()
applyConflictResolution worktreePath conflict = do
  mapM_ applyFile (pcFiles conflict)
  _ <- runGitInDir worktreePath ["add", "-A"]
  remaining <- getConflictedFiles worktreePath
  if null remaining
    then pure ()
    else throwIO (GitError "Conflict resolution did not clear all conflicts")
  where
    applyFile file = do
      let fullPath = worktreePath </> cfPath file
      if resPresent (cfResolution file)
        then applyPresent fullPath file
        else applyAbsent fullPath file

    applyPresent fullPath file =
      case resEncoding (cfResolution file) of
        EncodingGitlink -> do
          let hash = TextEnc.decodeUtf8 (trimBS (resContent (cfResolution file)))
          result <- runGitWithStatus ["-C", worktreePath, "update-index", "--cacheinfo", "160000", Text.unpack hash, cfPath file]
          if grExitCode result == 0
            then pure ()
            else throwIO (GitError "Failed to update gitlink index")
        EncodingText -> writeResolved fullPath (resContent (cfResolution file))
        EncodingBase64 ->
          case decodeBase64 (trimBS (resContent (cfResolution file))) of
            Left err -> throwIO (GitError err)
            Right decoded -> writeResolved fullPath decoded

    applyAbsent fullPath file =
      case resEncoding (cfResolution file) of
        EncodingGitlink -> do
          _ <- runGitWithStatus ["-C", worktreePath, "rm", "-f", "--cached", "--", cfPath file]
          removePath fullPath
        _ -> do
          exists <- doesPathExist fullPath
          if exists
            then removePath fullPath
            else pure ()

    writeResolved fullPath content = do
      exists <- doesDirectoryExist fullPath
      if exists
        then removePath fullPath
        else pure ()
      createDirectoryIfMissing True (takeDirectory fullPath)
      BS.writeFile fullPath content

removePath :: FilePath -> IO ()
removePath path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  if isDir
    then removePathForcibly path
    else if isFile
      then removeFile path
      else pure ()

finishCherryPick :: FilePath -> IO Bool
finishCherryPick worktreePath = do
  continueResult <- runGitWithStatusNoRerere ["-C", worktreePath, "cherry-pick", "--continue", "--no-edit"]
  if grExitCode continueResult == 0
    then pure True
    else if isEmptyCherryPickOutput (grStdout continueResult) || isEmptyCherryPickOutput (grStderr continueResult)
      then do
        skipResult <- runGitWithStatusNoRerere ["-C", worktreePath, "cherry-pick", "--skip"]
        pure (grExitCode skipResult == 0)
      else pure False

isEmptyCherryPickOutput :: ByteString -> Bool
isEmptyCherryPickOutput output =
  BSC.isInfixOf "previous cherry-pick is now empty" output ||
  BSC.isInfixOf "nothing to commit" output

findCherryPickConflict :: Plan -> [Bool] -> Text -> Text -> Maybe Int
findCherryPickConflict plan used branch commit =
  findIndex match (zip [0..] (planConflicts plan))
  where
    match (idx, conflict) =
      not (used !! idx)
        && pcKind conflict == ConflictCherryPick
        && pcBranch conflict == branch
        && pcCommit conflict == Just commit

findFixApplyConflict :: Plan -> [Bool] -> Text -> [FilePath] -> Maybe Int
findFixApplyConflict plan used branch conflictPaths =
  findIndex match (zip [0..] (planConflicts plan))
  where
    match (idx, conflict) =
      not (used !! idx)
        && pcKind conflict == ConflictFixApply
        && pcBranch conflict == branch
        && length (pcFiles conflict) == length conflictPaths
        && pathsMatch (pcFiles conflict) conflictPaths

pathsMatch :: [ConflictFile] -> [FilePath] -> Bool
pathsMatch files paths = all (\file -> cfPath file `elem` paths) files
