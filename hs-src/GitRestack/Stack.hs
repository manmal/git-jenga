{-# LANGUAGE OverloadedStrings #-}

module GitRestack.Stack
  ( analyzeStack
  , printTree
  , printJson
  ) where

import Control.Exception (throwIO)
import Data.ByteString.Char8 as BSC
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import GitRestack.Git
import GitRestack.Types
import GitRestack.Utils (trimBS)

analyzeStack :: IO Stack
analyzeStack = do
  headBranch <- runGit ["symbolic-ref", "--short", "HEAD"]
  headCommit <- runGit ["rev-parse", "HEAD"]
  baseBranch <- selectBaseBranch
  baseTip <- runGit ["rev-parse", Text.unpack baseBranch]
  baseCommit <- runGit ["merge-base", "HEAD", Text.unpack baseBranch]

  let headBranchT = TextEnc.decodeUtf8 (trimBS headBranch)
      headCommitT = TextEnc.decodeUtf8 (trimBS headCommit)
      baseTipT = TextEnc.decodeUtf8 (trimBS baseTip)
      baseCommitT = TextEnc.decodeUtf8 (trimBS baseCommit)

  let range = Text.unpack (baseCommitT <> "..HEAD")
  revList <- runGit ["rev-list", "--ancestry-path", "--reverse", range]
  let commits = filter (not . BSC.null) (BSC.lines (trimBS revList))

  branches <- buildBranches baseBranch commits
  pure Stack
    { stBranches = branches
    , stBaseBranch = baseBranch
    , stBaseCommit = baseCommitT
    , stBaseTip = baseTipT
    , stHeadBranch = headBranchT
    , stHeadCommit = headCommitT
    }

selectBaseBranch :: IO Text
selectBaseBranch = do
  hasDevelop <- checkBranchExists "develop"
  if hasDevelop
    then pure "develop"
    else do
      hasMain <- checkBranchExists "main"
      if hasMain
        then pure "main"
        else throwIO (GitError "Base branch not found")

checkBranchExists :: String -> IO Bool
checkBranchExists branch = do
  result <- runGitWithStatus ["rev-parse", "--verify", branch]
  pure (grExitCode result == 0)

buildBranches :: Text -> [BSC.ByteString] -> IO [StackBranch]
buildBranches baseBranch commits = go commits (Just baseBranch) 0 []
  where
    go [] _ _ acc = pure (reverse acc)
    go (sha:rest) currentParent count acc = do
      let commitSha = TextEnc.decodeUtf8 sha
      branchAtCommit <- getBranchAtCommit commitSha
      case branchAtCommit of
        Nothing -> go rest currentParent (count + 1) acc
        Just branchName -> do
          let branch = StackBranch
                { sbName = branchName
                , sbCommitSha = commitSha
                , sbParentBranch = currentParent
                , sbCommitsFromParent = count + 1
                , sbNeedsFix = False
                , sbFix = Nothing
                , sbStepStatus = StepPending
                }
          go rest (Just branchName) 0 (branch : acc)

getBranchAtCommit :: Text -> IO (Maybe Text)
getBranchAtCommit commitSha = do
  output <- runGit ["branch", "--points-at", Text.unpack commitSha, "--format=%(refname:short)"]
  let branches = map TextEnc.decodeUtf8 (BSC.lines (trimBS output))
  pure (find (Text.isPrefixOf "feature/") branches)

printTree :: Stack -> IO ()
printTree stack = do
  putStrLn (Text.unpack (stBaseBranch stack) <> " (" <> shortSha (stBaseCommit stack) <> ")")
  let total = length (stBranches stack)
  mapM_ (printBranch total) (zip [0..] (stBranches stack))
  where
    shortSha sha = Text.unpack (Text.take 7 sha)
    printBranch total (idx, branch) = do
      let prefix = if idx == total - 1 then "`-- " else "|-- "
      putStrLn (prefix <> Text.unpack (sbName branch) <> " (" <> shortSha (sbCommitSha branch) <> ") +" <> show (sbCommitsFromParent branch) <> " commits")

printJson :: Stack -> IO ()
printJson stack = do
  putStrLn "{" 
  putStrLn ("  \"base_branch\": \"" <> Text.unpack (stBaseBranch stack) <> "\",")
  putStrLn ("  \"base_commit\": \"" <> Text.unpack (stBaseCommit stack) <> "\",")
  putStrLn ("  \"base_tip\": \"" <> Text.unpack (stBaseTip stack) <> "\",")
  putStrLn ("  \"head_branch\": \"" <> Text.unpack (stHeadBranch stack) <> "\",")
  putStrLn ("  \"head_commit\": \"" <> Text.unpack (stHeadCommit stack) <> "\",")
  putStrLn "  \"branches\": ["
  mapM_ printBranchJson (zip [0..] (stBranches stack))
  putStrLn "  ]"
  putStrLn "}"
  where
    total = length (stBranches stack)
    printBranchJson (idx, branch) = do
      putStrLn "    {"
      putStrLn ("      \"name\": \"" <> Text.unpack (sbName branch) <> "\",")
      putStrLn ("      \"commit\": \"" <> Text.unpack (sbCommitSha branch) <> "\",")
      case sbParentBranch branch of
        Just parent -> putStrLn ("      \"parent_branch\": \"" <> Text.unpack parent <> "\",")
        Nothing -> putStrLn "      \"parent_branch\": null,"
      putStrLn ("      \"commits_from_parent\": " <> show (sbCommitsFromParent branch))
      if idx < total - 1
        then putStrLn "    },"
        else putStrLn "    }"
