{-# LANGUAGE OverloadedStrings #-}

module GitRestack.Yaml
  ( emitPlan
  , emitState
  , parsePlan
  , parseState
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Yaml as Yaml
import GitRestack.Types

emitPlan :: Plan -> ByteString
emitPlan plan = BL.toStrict (toLazyByteString builder)
  where
    builder = mconcat
      [ builderText "# git-restack restacking plan\n"
      , builderText "version: " <> builderText (Text.pack (show (planVersion plan))) <> builderText "\n"
      , builderText "generated: \"" <> builderText (escapeYaml (planGenerated plan)) <> builderText "\"\n"
      , builderText "repository: \"" <> builderText (escapeYaml (planRepository plan)) <> builderText "\"\n"
      , maybe mempty (\cmd -> builderText "verify_cmd: \"" <> builderText (escapeYaml cmd) <> builderText "\"\n") (planVerifyCmd plan)
      , builderText "\n"
      , builderText "# STRICT VALIDATION\n"
      , builderText "# If this list is not empty, 'exec' will refuse to run.\n"
      , builderText "# User must move entries to appropriate branch fix blocks or delete files.\n"
      , builderText "errors:\n"
      , if null (planErrors plan)
          then builderText "  []\n"
          else mconcat (map renderError (planErrors plan))
      , builderText "\n"
      , case planSimulation plan of
          Nothing -> mempty
          Just sim -> renderSimulation sim <> builderText "\n"
      , builderText "# Source state snapshot\n"
      , builderText "source:\n"
      , builderText "  head_branch: \"" <> builderText (escapeYaml (stHeadBranch (planStack plan))) <> builderText "\"\n"
      , builderText "  head_commit: \"" <> builderText (escapeYaml (stHeadCommit (planStack plan))) <> builderText "\"\n"
      , builderText "  base_branch: \"" <> builderText (escapeYaml (stBaseBranch (planStack plan))) <> builderText "\"\n"
      , builderText "  base_commit: \"" <> builderText (escapeYaml (stBaseCommit (planStack plan))) <> builderText "\"\n"
      , builderText "  base_tip: \"" <> builderText (escapeYaml (stBaseTip (planStack plan))) <> builderText "\"\n\n"
      , builderText "# Branch stack (ordered from base to HEAD)\n"
      , builderText "stack:\n"
      , mconcat (map renderBranch (stBranches (planStack plan)))
      , builderText "# Conflict resolutions\n"
      , builderText "conflicts:\n"
      , if null (planConflicts plan)
          then builderText "  []\n"
          else mconcat (map renderConflict (planConflicts plan))
      , builderText "\n"
      ]

renderError :: PlanError -> Builder
renderError err = mconcat
  [ builderText "  - type: " <> builderText (errorTypeToText (peType err)) <> builderText "\n"
  , builderText "    path: \"" <> builderText (escapeYaml (Text.pack (pePath err))) <> builderText "\"\n"
  , builderText "    message: \"" <> builderText (escapeYaml (peMessage err)) <> builderText "\"\n"
  ]

renderSimulation :: PlanSimulation -> Builder
renderSimulation sim = mconcat
  [ builderText "# Simulation metadata\n"
  , builderText "simulation:\n"
  , builderText "  mode: " <> builderText (planModeToText (simMode sim)) <> builderText "\n"
  , builderText "  worktree_path: " <> renderMaybeQuoted (simWorktreePath sim)
  , builderText "  plan_branch: " <> renderMaybeQuotedText (simPlanBranch sim)
  , builderText "  backup_branches:\n"
  , if null (simBackupBranches sim)
      then builderText "    []\n"
      else mconcat (map renderBackup (simBackupBranches sim))
  ]

renderBackup :: BackupBranch -> Builder
renderBackup backup = mconcat
  [ builderText "    - source: \"" <> builderText (escapeYaml (backupSource backup)) <> builderText "\"\n"
  , builderText "      backup: \"" <> builderText (escapeYaml (backupName backup)) <> builderText "\"\n"
  ]

renderBranch :: StackBranch -> Builder
renderBranch branch = mconcat
  [ builderText "  - branch: \"" <> builderText (escapeYaml (sbName branch)) <> builderText "\"\n"
  , builderText "    commit: \"" <> builderText (escapeYaml (sbCommitSha branch)) <> builderText "\"\n"
  , case sbParentBranch branch of
      Nothing -> builderText "    parent_branch: null\n"
      Just parent -> builderText "    parent_branch: \"" <> builderText (escapeYaml parent) <> builderText "\"\n"
  , builderText "    commits_from_parent: " <> builderText (Text.pack (show (sbCommitsFromParent branch))) <> builderText "\n"
  , builderText "    needs_fix: " <> builderText (if sbNeedsFix branch then "true" else "false") <> builderText "\n"
  , builderText "    step_status: " <> builderText (stepStatusToText (sbStepStatus branch)) <> builderText "\n"
  , case sbFix branch of
      Nothing -> builderText "\n"
      Just fix -> renderFix fix
  ]

renderFix :: Fix -> Builder
renderFix fix = mconcat
  [ builderText "    fix:\n"
  , builderText "      commit_message: |\n"
  , renderLiteralBlock 8 (TextEnc.encodeUtf8 (fixCommitMessage fix))
  , builderText "      files:\n"
  , mconcat (map renderFixFile (fixFiles fix))
  , builderText "\n"
  ]

renderFixFile :: FileChange -> Builder
renderFixFile fileChange = mconcat
  [ builderText "        - path: \"" <> builderText (escapeYaml (Text.pack (fcPath fileChange))) <> builderText "\"\n"
  , builderText "          change_type: " <> builderText (changeTypeToText (fcChangeType fileChange)) <> builderText "\n"
  , builderText "          staged: " <> builderText (if fcStaged fileChange then "true" else "false") <> builderText "\n"
  , builderText "          diff: |\n"
  , renderLiteralBlock 12 (fcDiff fileChange)
  ]

renderConflict :: PlanConflict -> Builder
renderConflict conflict = mconcat
  [ builderText "  - kind: " <> builderText (conflictKindToText (pcKind conflict)) <> builderText "\n"
  , builderText "    branch: \"" <> builderText (escapeYaml (pcBranch conflict)) <> builderText "\"\n"
  , case pcCommit conflict of
      Nothing -> builderText "    commit: null\n"
      Just commit -> builderText "    commit: \"" <> builderText (escapeYaml commit) <> builderText "\"\n"
  , case pcSubject conflict of
      Nothing -> builderText "    subject: null\n"
      Just subject -> builderText "    subject: \"" <> builderText (escapeYaml subject) <> builderText "\"\n"
  , builderText "    files:\n"
  , mconcat (map renderConflictFile (pcFiles conflict))
  ]

renderConflictFile :: ConflictFile -> Builder
renderConflictFile file = mconcat
  [ builderText "      - path: \"" <> builderText (escapeYaml (Text.pack (cfPath file))) <> builderText "\"\n"
  , builderText "        conflict_diff: |\n"
  , renderLiteralBlock 10 (cfConflictDiff file)
  , builderText "        resolution:\n"
  , builderText "          present: " <> builderText (if resPresent (cfResolution file) then "true" else "false") <> builderText "\n"
  , builderText "          encoding: " <> builderText (resolutionEncodingToText (resEncoding (cfResolution file))) <> builderText "\n"
  , builderText "          content: |\n"
  , renderLiteralBlock 12 (resContent (cfResolution file))
  ]

renderLiteralBlock :: Int -> ByteString -> Builder
renderLiteralBlock indent bs = mconcat (map renderLine (BSC.split '\n' bs))
  where
    pad = BSC.replicate indent ' '
    renderLine line = builderByteString pad <> builderByteString line <> builderText "\n"

renderMaybeQuoted :: Maybe FilePath -> Builder
renderMaybeQuoted Nothing = builderText "null\n"
renderMaybeQuoted (Just path) = builderText "\"" <> builderText (escapeYaml (Text.pack path)) <> builderText "\"\n"

renderMaybeQuotedText :: Maybe Text -> Builder
renderMaybeQuotedText Nothing = builderText "null\n"
renderMaybeQuotedText (Just value) = builderText "\"" <> builderText (escapeYaml value) <> builderText "\"\n"

escapeYaml :: Text -> Text
escapeYaml = Text.concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c = Text.singleton c

builderText :: Text -> Builder
builderText = byteString . TextEnc.encodeUtf8

builderByteString :: ByteString -> Builder
builderByteString = byteString

emitState :: ExecutionState -> ByteString
emitState state = BL.toStrict (toLazyByteString builder)
  where
    builder = mconcat
      [ builderText "{\n"
      , builderText "  \"plan_file\": \"" <> builderText (escapeYaml (Text.pack (esPlanFile state))) <> builderText "\",\n"
      , builderText "  \"plan_hash\": \"" <> builderText (escapeYaml (esPlanHash state)) <> builderText "\",\n"
      , builderText "  \"worktree_path\": \"" <> builderText (escapeYaml (Text.pack (esWorktreePath state))) <> builderText "\",\n"
      , builderText "  \"current_step_index\": " <> builderText (Text.pack (show (esCurrentStepIndex state))) <> builderText ",\n"
      , builderText "  \"current_commit_index\": " <> builderText (Text.pack (show (esCurrentCommitIndex state))) <> builderText ",\n"
      , builderText "  \"started_at\": \"" <> builderText (escapeYaml (esStartedAt state)) <> builderText "\",\n"
      , builderText "  \"last_updated\": \"" <> builderText (escapeYaml (esLastUpdated state)) <> builderText "\",\n"
      , builderText "  \"status\": \"" <> builderText (executionStatusToText (esStatus state)) <> builderText "\",\n"
      , builderText "  \"mode\": \"" <> builderText (executionModeToText (esMode state)) <> builderText "\",\n"
      , builderText "  \"completed_branches\": [" <> renderCompleted (esCompletedBranches state) <> builderText "],\n"
      , builderText "  \"verify_cmd\": " <> renderVerify (esVerifyCmd state) <> builderText "\n"
      , builderText "}\n"
      ]
    renderCompleted branches = mconcat (zipWith renderBranch [0 :: Int ..] branches)
    renderBranch idx branch =
      let comma = if idx < length (esCompletedBranches state) - 1 then ", " else ""
      in builderText "\"" <> builderText (escapeYaml branch) <> builderText "\"" <> builderText comma
    renderVerify Nothing = builderText "null"
    renderVerify (Just cmd) = builderText "\"" <> builderText (escapeYaml cmd) <> builderText "\""

parsePlan :: ByteString -> Either String Plan
parsePlan content =
  case Yaml.decodeEither' content of
    Left err -> Left (show err)
    Right value -> parseEither parsePlanValue value

parsePlanValue :: Value -> Parser Plan
parsePlanValue = withObject "Plan" $ \obj -> do
  version <- obj .:? "version" .!= 1
  generated <- obj .:? "generated" .!= ""
  repository <- obj .:? "repository" .!= ""
  verifyCmd <- obj .:? "verify_cmd"
  simulation <- obj .:? "simulation"
  errors <- obj .:? "errors" .!= []
  conflicts <- obj .:? "conflicts" .!= []
  source <- obj .: "source"
  stackBranches <- obj .:? "stack" .!= []
  let stack = Stack
        { stBranches = stackBranches
        , stBaseBranch = srcBaseBranch source
        , stBaseCommit = srcBaseCommit source
        , stBaseTip = if Text.null (srcBaseTip source) then srcBaseCommit source else srcBaseTip source
        , stHeadBranch = srcHeadBranch source
        , stHeadCommit = srcHeadCommit source
        }
  pure Plan
    { planVersion = version
    , planGenerated = generated
    , planRepository = repository
    , planVerifyCmd = verifyCmd
    , planSimulation = simulation
    , planErrors = errors
    , planConflicts = conflicts
    , planStack = stack
    }

parseState :: ByteString -> Either String ExecutionState
parseState content =
  case eitherDecodeStrict' content of
    Left err -> Left err
    Right state -> Right state


data PlanSource = PlanSource
  { srcHeadBranch :: Text
  , srcHeadCommit :: Text
  , srcBaseBranch :: Text
  , srcBaseCommit :: Text
  , srcBaseTip :: Text
  }

instance FromJSON PlanSource where
  parseJSON = withObject "PlanSource" $ \obj -> do
    headBranch <- obj .:? "head_branch" .!= ""
    headCommit <- obj .:? "head_commit" .!= ""
    baseBranch <- obj .:? "base_branch" .!= ""
    baseCommit <- obj .:? "base_commit" .!= ""
    baseTip <- obj .:? "base_tip" .!= ""
    pure PlanSource
      { srcHeadBranch = headBranch
      , srcHeadCommit = headCommit
      , srcBaseBranch = baseBranch
      , srcBaseCommit = baseCommit
      , srcBaseTip = baseTip
      }

instance FromJSON PlanSimulation where
  parseJSON = withObject "PlanSimulation" $ \obj -> do
    mode <- planModeFromText <$> (obj .:? "mode" .!= "worktree")
    worktreePath <- obj .:? "worktree_path"
    planBranch <- obj .:? "plan_branch"
    backups <- obj .:? "backup_branches" .!= []
    pure PlanSimulation
      { simMode = mode
      , simWorktreePath = worktreePath
      , simPlanBranch = planBranch
      , simBackupBranches = backups
      }

instance FromJSON BackupBranch where
  parseJSON = withObject "BackupBranch" $ \obj -> do
    source <- obj .:? "source" .!= ""
    backup <- obj .:? "backup" .!= ""
    pure BackupBranch
      { backupSource = source
      , backupName = backup
      }

instance FromJSON PlanError where
  parseJSON = withObject "PlanError" $ \obj -> do
    errorType <- errorTypeFromText <$> (obj .:? "type" .!= "unmapped_file")
    path <- obj .:? "path" .!= ""
    message <- obj .:? "message" .!= ""
    pure PlanError
      { peType = errorType
      , pePath = Text.unpack path
      , peMessage = message
      }

instance FromJSON StackBranch where
  parseJSON = withObject "StackBranch" $ \obj -> do
    name <- obj .:? "branch" .!= ""
    commit <- obj .:? "commit" .!= ""
    parent <- obj .:? "parent_branch"
    commitsFromParent <- obj .:? "commits_from_parent" .!= 0
    needsFix <- obj .:? "needs_fix" .!= False
    stepStatus <- stepStatusFromText <$> (obj .:? "step_status" .!= "pending")
    fix <- obj .:? "fix"
    pure StackBranch
      { sbName = name
      , sbCommitSha = commit
      , sbParentBranch = parent
      , sbCommitsFromParent = commitsFromParent
      , sbNeedsFix = needsFix
      , sbFix = fix
      , sbStepStatus = stepStatus
      }

instance FromJSON Fix where
  parseJSON = withObject "Fix" $ \obj -> do
    commitMessage <- obj .:? "commit_message" .!= ""
    files <- obj .:? "files" .!= []
    pure Fix
      { fixCommitMessage = commitMessage
      , fixFiles = files
      }

instance FromJSON FileChange where
  parseJSON = withObject "FileChange" $ \obj -> do
    path <- obj .:? "path" .!= ""
    changeType <- changeTypeFromText <$> (obj .:? "change_type" .!= "modified")
    staged <- obj .:? "staged" .!= True
    diffText <- obj .:? "diff" .!= ""
    pure FileChange
      { fcPath = Text.unpack path
      , fcChangeType = changeType
      , fcDiff = TextEnc.encodeUtf8 diffText
      , fcStaged = staged
      }

instance FromJSON PlanConflict where
  parseJSON = withObject "PlanConflict" $ \obj -> do
    kind <- conflictKindFromText <$> (obj .:? "kind" .!= "cherry_pick")
    branch <- obj .:? "branch" .!= ""
    commit <- obj .:? "commit"
    subject <- obj .:? "subject"
    files <- obj .:? "files" .!= []
    pure PlanConflict
      { pcKind = kind
      , pcBranch = branch
      , pcCommit = commit
      , pcSubject = subject
      , pcFiles = files
      }

instance FromJSON ConflictFile where
  parseJSON = withObject "ConflictFile" $ \obj -> do
    path <- obj .:? "path" .!= ""
    conflictDiff <- obj .:? "conflict_diff" .!= ""
    resolution <- obj .: "resolution"
    pure ConflictFile
      { cfPath = Text.unpack path
      , cfConflictDiff = TextEnc.encodeUtf8 conflictDiff
      , cfResolution = resolution
      }

instance FromJSON ConflictResolution where
  parseJSON = withObject "ConflictResolution" $ \obj -> do
    present <- obj .:? "present" .!= False
    encoding <- resolutionEncodingFromText <$> (obj .:? "encoding" .!= "text")
    content <- obj .:? "content" .!= ""
    pure ConflictResolution
      { resPresent = present
      , resEncoding = encoding
      , resContent = TextEnc.encodeUtf8 content
      }

instance FromJSON ExecutionState where
  parseJSON = withObject "ExecutionState" $ \obj -> do
    planFile <- obj .:? "plan_file" .!= ""
    planHash <- obj .:? "plan_hash" .!= ""
    worktreePath <- obj .:? "worktree_path" .!= ""
    currentStepIndex <- obj .:? "current_step_index" .!= 0
    currentCommitIndex <- obj .:? "current_commit_index" .!= 0
    startedAt <- obj .:? "started_at" .!= ""
    lastUpdated <- obj .:? "last_updated" .!= ""
    status <- executionStatusFromText <$> (obj .:? "status" .!= "pending")
    completedBranches <- obj .:? "completed_branches" .!= []
    verifyCmd <- obj .:? "verify_cmd"
    mode <- executionModeFromText <$> (obj .:? "mode" .!= "exec")
    pure ExecutionState
      { esPlanFile = Text.unpack planFile
      , esPlanHash = planHash
      , esWorktreePath = Text.unpack worktreePath
      , esCurrentStepIndex = currentStepIndex
      , esCurrentCommitIndex = currentCommitIndex
      , esStartedAt = startedAt
      , esLastUpdated = lastUpdated
      , esStatus = status
      , esCompletedBranches = completedBranches
      , esVerifyCmd = verifyCmd
      , esMode = mode
      }
