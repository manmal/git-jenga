{-# LANGUAGE OverloadedStrings #-}

module GitRestack.Git
  ( GitResult(..)
  , GitError(..)
  , runGit
  , runGitWithStatus
  , runGitWithStatusNoRerere
  , runGitInDir
  , getCwd
  ) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)


data GitResult = GitResult
  { grStdout :: ByteString
  , grStderr :: ByteString
  , grExitCode :: Int
  } deriving (Eq, Show)

newtype GitError = GitError String deriving (Eq, Show)
instance Exception GitError

runGitWithStatus :: [String] -> IO GitResult
runGitWithStatus args = do
  (exitCode, stdoutStr, stderrStr) <- readProcessWithExitCode "git" args ""
  let code = case exitCode of
        ExitSuccess -> 0
        ExitFailure n -> n
  pure GitResult
    { grStdout = BSC.pack stdoutStr
    , grStderr = BSC.pack stderrStr
    , grExitCode = code
    }

runGit :: [String] -> IO ByteString
runGit args = do
  result <- runGitWithStatus args
  if grExitCode result == 0
    then pure (grStdout result)
    else throwIO (GitError (BSC.unpack (grStderr result)))

runGitWithStatusNoRerere :: [String] -> IO GitResult
runGitWithStatusNoRerere args =
  runGitWithStatus ("-c" : "rerere.enabled=false" : "-c" : "rerere.autoUpdate=false" : args)

runGitInDir :: FilePath -> [String] -> IO ByteString
runGitInDir dir args = runGit ("-C" : dir : args)

getCwd :: IO FilePath
getCwd = getCurrentDirectory
