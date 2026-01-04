{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitRestack.Status
  ( StatusOptions(..)
  , runStatus
  ) where

import Control.Exception (catch)
import Control.Monad (forM_, when)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as Text
import GitRestack.Types
import GitRestack.Yaml
import System.Directory
import System.Exit (ExitCode(..), exitWith)


data StatusOptions = StatusOptions
  { soPlanFile :: Maybe FilePath
  }

runStatus :: StatusOptions -> IO ()
runStatus opts = do
  let stateFile = ".git/git-restack/state.json"
  hasState <- doesFileExist stateFile
  if hasState
    then showState stateFile
    else case soPlanFile opts of
      Nothing -> do
        putStrLn "No execution in progress."
        putStrLn "Run 'git-restack plan' to generate a plan."
      Just planFile -> showPlanStatus planFile

showState :: FilePath -> IO ()
showState stateFile = do
  content <- BSC.readFile stateFile
  case parseState content of
    Left _ -> do
      putStrLn "Error: Could not parse state file."
      exitWith (ExitFailure 1)
    Right state -> do
      putStrLn "\nExecution Status"
      putStrLn "------------------------------\n"

      putStrLn ("Plan file:    " <> esPlanFile state)
      putStrLn ("Worktree:     " <> esWorktreePath state)
      putStrLn ("Started:      " <> Text.unpack (esStartedAt state))
      putStrLn ("Last updated: " <> Text.unpack (esLastUpdated state))
      putStrLn ("Status:       " <> Text.unpack (executionStatusToText (esStatus state)))
      putStrLn ("Progress:     " <> show (length (esCompletedBranches state)) <> " branches completed")

      when (not (null (esCompletedBranches state))) $ do
        putStrLn "\nCompleted branches:"
        forM_ (esCompletedBranches state) $ \branch ->
          putStrLn ("  [OK] " <> Text.unpack branch)

      putStrLn ""

      case esStatus state of
        StatusConflict -> do
          putStrLn "Action required: Resolve conflicts in worktree."
          putStrLn ("  cd " <> esWorktreePath state)
          putStrLn "  git add <resolved_files>"
          putStrLn "  git cherry-pick --continue"
          putStrLn "  cd -"
          putStrLn ("  git-restack exec " <> esPlanFile state <> " --continue")
        StatusInProgress -> do
          putStrLn "Execution in progress."
          putStrLn ("Resume with: git-restack exec " <> esPlanFile state <> " --continue")
          putStrLn ("Or abort:    git-restack exec " <> esPlanFile state <> " --abort")
        StatusCompleted ->
          putStrLn "Execution completed successfully."
        StatusFailed -> do
          putStrLn "Execution failed."
          putStrLn ("Abort with: git-restack exec " <> esPlanFile state <> " --abort")
        _ -> pure ()

showPlanStatus :: FilePath -> IO ()
showPlanStatus planFile = do
  content <- BSC.readFile planFile `catch` \(_ :: IOError) -> do
    putStrLn ("Error: Could not read plan file '" <> planFile <> "'")
    exitWith (ExitFailure 1)
  case parsePlan content of
    Left _ -> do
      putStrLn "Error: Could not parse plan file."
      exitWith (ExitFailure 1)
    Right plan -> do
      putStrLn "\nPlan Status"
      putStrLn "------------------------------\n"

      putStrLn ("Plan file:  " <> planFile)
      putStrLn ("Generated:  " <> Text.unpack (planGenerated plan))
      putStrLn ("Repository: " <> Text.unpack (planRepository plan))
      putStrLn ""

      when (not (null (planErrors plan))) $ do
        putStrLn ("Errors: " <> show (length (planErrors plan)))
        forM_ (planErrors plan) $ \err ->
          putStrLn ("  - [" <> Text.unpack (errorTypeToText (peType err)) <> "] " <> pePath err)
        putStrLn ""

      let needsFix = filter sbNeedsFix (stBranches (planStack plan))
      let totalFiles = sum (map (maybe 0 (length . fixFiles) . sbFix) needsFix)

      putStrLn ("Branches: " <> show (length (stBranches (planStack plan))))
      putStrLn ("Conflicts: " <> show (length (planConflicts plan)))
      putStrLn ("Need fixes: " <> show (length needsFix))
      putStrLn ("Total files: " <> show totalFiles)

      putStrLn "\nBranch details:"
      forM_ (stBranches (planStack plan)) $ \branch -> do
        if sbNeedsFix branch
          then putStrLn ("  [FIX] " <> Text.unpack (sbName branch) <> " (" <> show (maybe 0 (length . fixFiles) (sbFix branch)) <> " files)")
          else putStrLn ("  [OK]  " <> Text.unpack (sbName branch))

      putStrLn ""

      if not (null (planErrors plan))
        then putStrLn "Fix errors in plan before executing."
        else if not (null needsFix)
          then putStrLn ("Ready to execute: git-restack exec " <> planFile)
          else putStrLn "No changes needed."
