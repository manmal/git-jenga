{-# LANGUAGE OverloadedStrings #-}

module GitRestack.Diff
  ( ChangedFile(..)
  , getConflictedFiles
  , getStagedFiles
  , getUnstagedFiles
  , getFileDiff
  , getLastCommitForFile
  , isCommitInAncestry
  , findBranchForCommit
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import GitRestack.Git
import GitRestack.Types
import GitRestack.Utils (trimBS)


data ChangedFile = ChangedFile
  { changedPath :: FilePath
  , changedType :: ChangeType
  , changedStaged :: Bool
  , changedLastCommit :: Maybe Text
  } deriving (Eq, Show)

getConflictedFiles :: FilePath -> IO [FilePath]
getConflictedFiles repoPath = do
  result <- runGitWithStatus ["-C", repoPath, "diff", "--name-only", "--diff-filter=U"]
  if grExitCode result /= 0 || BSC.null (grStdout result)
    then pure []
    else pure (filter (not . null) (map BSC.unpack (BSC.lines (trimBS (grStdout result)))))

getStagedFiles :: IO [ChangedFile]
getStagedFiles = getChangedFiles True

getUnstagedFiles :: IO [ChangedFile]
getUnstagedFiles = getChangedFiles False

getChangedFiles :: Bool -> IO [ChangedFile]
getChangedFiles staged = do
  let args = if staged
        then ["diff", "--cached", "--name-status"]
        else ["diff", "--name-status"]
  result <- runGitWithStatus args
  if grExitCode result /= 0
    then pure []
    else pure (catMaybes (map (parseLine staged) (BSC.lines (trimBS (grStdout result)))))
  where
    parseLine isStaged line
      | BSC.length line < 2 = Nothing
      | otherwise =
          let status = BSC.head line
              pathStart = if BSC.length line > 1 && BSC.index line 1 == '\t' then 2 else 1
              rawPath = BSC.drop pathStart line
              pathTrim = BSC.unpack (trimBS (takeUntilTab rawPath))
              changeType = case status of
                'A' -> ChangeAdded
                'D' -> ChangeDeleted
                'M' -> ChangeModified
                _ -> ChangeModified
          in if null pathTrim
              then Nothing
              else Just ChangedFile
                { changedPath = pathTrim
                , changedType = changeType
                , changedStaged = isStaged
                , changedLastCommit = Nothing
                }
    takeUntilTab bs = case BSC.elemIndex '\t' bs of
      Nothing -> bs
      Just idx -> BSC.take idx bs

getFileDiff :: FilePath -> Bool -> IO ByteString
getFileDiff path staged = do
  let args = if staged
        then ["diff", "--cached", "--", path]
        else ["diff", "--", path]
  result <- runGitWithStatus args
  if grExitCode result /= 0
    then pure ""
    else pure (grStdout result)

getLastCommitForFile :: FilePath -> IO (Maybe Text)
getLastCommitForFile path = do
  result <- runGitWithStatus ["log", "-1", "--format=%H", "--", path]
  if grExitCode result /= 0 || BSC.null (trimBS (grStdout result))
    then pure Nothing
    else pure (Just (TextEnc.decodeUtf8 (trimBS (grStdout result))))

isCommitInAncestry :: Text -> Text -> IO Bool
isCommitInAncestry commit baseCommit = do
  let range = Text.unpack (baseCommit <> "..HEAD")
  result <- runGitWithStatus ["rev-list", "--ancestry-path", range]
  if grExitCode result /= 0
    then pure False
    else pure (commit `elem` map TextEnc.decodeUtf8 (BSC.lines (grStdout result)))

findBranchForCommit :: Text -> Stack -> IO (Maybe Text)
findBranchForCommit commit stack = go (stBranches stack)
  where
    go [] = pure Nothing
    go (branch:rest) = do
      isAncestorBranch <- isAncestor commit (sbCommitSha branch)
      if not isAncestorBranch
        then go rest
        else case sbParentBranch branch of
          Nothing -> pure (Just (sbName branch))
          Just parentName -> do
            parentCommit <- getParentCommit parentName stack
            case parentCommit of
              Nothing -> go rest
              Just pc -> do
                isAfterParent <- isAncestor pc commit
                if isAfterParent
                  then pure (Just (sbName branch))
                  else go rest

isAncestor :: Text -> Text -> IO Bool
isAncestor ancestor descendant = do
  result <- runGitWithStatus ["merge-base", "--is-ancestor", Text.unpack ancestor, Text.unpack descendant]
  pure (grExitCode result == 0)

getParentCommit :: Text -> Stack -> IO (Maybe Text)
getParentCommit parentName stack
  | parentName == stBaseBranch stack = pure (Just (stBaseCommit stack))
  | otherwise = pure (sbCommitSha <$> find ((== parentName) . sbName) (stBranches stack))
