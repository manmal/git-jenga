{-# LANGUAGE OverloadedStrings #-}

module GitRestack.Types
  ( StackBranch(..)
  , Fix(..)
  , PlanMode(..)
  , BackupBranch(..)
  , PlanSimulation(..)
  , ConflictKind(..)
  , ResolutionEncoding(..)
  , ConflictResolution(..)
  , ConflictFile(..)
  , PlanConflict(..)
  , FileChange(..)
  , ChangeType(..)
  , StepStatus(..)
  , Stack(..)
  , PlanError(..)
  , ErrorType(..)
  , Plan(..)
  , ExecutionMode(..)
  , ExecutionState(..)
  , ExecutionStatus(..)
  , changeTypeToText
  , changeTypeFromText
  , planModeToText
  , planModeFromText
  , conflictKindToText
  , conflictKindFromText
  , resolutionEncodingToText
  , resolutionEncodingFromText
  , stepStatusToText
  , stepStatusFromText
  , executionModeToText
  , executionModeFromText
  , executionStatusToText
  , executionStatusFromText
  , errorTypeToText
  , errorTypeFromText
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text


data StackBranch = StackBranch
  { sbName :: Text
  , sbCommitSha :: Text
  , sbParentBranch :: Maybe Text
  , sbCommitsFromParent :: Int
  , sbNeedsFix :: Bool
  , sbFix :: Maybe Fix
  , sbStepStatus :: StepStatus
  } deriving (Eq, Show)


data Fix = Fix
  { fixCommitMessage :: Text
  , fixFiles :: [FileChange]
  } deriving (Eq, Show)


data PlanMode
  = PlanModeWorktree
  | PlanModeDirect
  deriving (Eq, Show)


data BackupBranch = BackupBranch
  { backupSource :: Text
  , backupName :: Text
  } deriving (Eq, Show)


data PlanSimulation = PlanSimulation
  { simMode :: PlanMode
  , simWorktreePath :: Maybe FilePath
  , simPlanBranch :: Maybe Text
  , simBackupBranches :: [BackupBranch]
  } deriving (Eq, Show)


data ConflictKind
  = ConflictCherryPick
  | ConflictFixApply
  deriving (Eq, Show)


data ResolutionEncoding
  = EncodingText
  | EncodingBase64
  | EncodingGitlink
  deriving (Eq, Show)


data ConflictResolution = ConflictResolution
  { resPresent :: Bool
  , resEncoding :: ResolutionEncoding
  , resContent :: ByteString
  } deriving (Eq, Show)


data ConflictFile = ConflictFile
  { cfPath :: FilePath
  , cfConflictDiff :: ByteString
  , cfResolution :: ConflictResolution
  } deriving (Eq, Show)


data PlanConflict = PlanConflict
  { pcKind :: ConflictKind
  , pcBranch :: Text
  , pcCommit :: Maybe Text
  , pcSubject :: Maybe Text
  , pcFiles :: [ConflictFile]
  } deriving (Eq, Show)


data FileChange = FileChange
  { fcPath :: FilePath
  , fcChangeType :: ChangeType
  , fcDiff :: ByteString
  , fcStaged :: Bool
  } deriving (Eq, Show)


data ChangeType
  = ChangeModified
  | ChangeDeleted
  | ChangeAdded
  deriving (Eq, Show)


data StepStatus
  = StepPending
  | StepCherryPicked
  | StepFixed
  | StepVerified
  | StepSkipped
  deriving (Eq, Show)


data Stack = Stack
  { stBranches :: [StackBranch]
  , stBaseBranch :: Text
  , stBaseCommit :: Text
  , stBaseTip :: Text
  , stHeadBranch :: Text
  , stHeadCommit :: Text
  } deriving (Eq, Show)


data PlanError = PlanError
  { peType :: ErrorType
  , pePath :: FilePath
  , peMessage :: Text
  } deriving (Eq, Show)


data ErrorType
  = ErrorUnmappedFile
  | ErrorOutsideAncestry
  | ErrorAmbiguousBranch
  deriving (Eq, Show)


data Plan = Plan
  { planVersion :: Int
  , planGenerated :: Text
  , planRepository :: Text
  , planVerifyCmd :: Maybe Text
  , planSimulation :: Maybe PlanSimulation
  , planErrors :: [PlanError]
  , planConflicts :: [PlanConflict]
  , planStack :: Stack
  } deriving (Eq, Show)


data ExecutionMode
  = ExecutionExec
  | ExecutionStep
  deriving (Eq, Show)


data ExecutionState = ExecutionState
  { esPlanFile :: FilePath
  , esPlanHash :: Text
  , esWorktreePath :: FilePath
  , esCurrentStepIndex :: Int
  , esCurrentCommitIndex :: Int
  , esStartedAt :: Text
  , esLastUpdated :: Text
  , esStatus :: ExecutionStatus
  , esCompletedBranches :: [Text]
  , esVerifyCmd :: Maybe Text
  , esMode :: ExecutionMode
  } deriving (Eq, Show)


data ExecutionStatus
  = StatusPending
  | StatusInProgress
  | StatusConflict
  | StatusCompleted
  | StatusFailed
  | StatusAborted
  deriving (Eq, Show)


changeTypeToText :: ChangeType -> Text
changeTypeToText ChangeModified = "modified"
changeTypeToText ChangeDeleted = "deleted"
changeTypeToText ChangeAdded = "added"

changeTypeFromText :: Text -> ChangeType
changeTypeFromText "added" = ChangeAdded
changeTypeFromText "deleted" = ChangeDeleted
changeTypeFromText _ = ChangeModified

planModeToText :: PlanMode -> Text
planModeToText PlanModeWorktree = "worktree"
planModeToText PlanModeDirect = "direct"

planModeFromText :: Text -> PlanMode
planModeFromText "direct" = PlanModeDirect
planModeFromText _ = PlanModeWorktree

conflictKindToText :: ConflictKind -> Text
conflictKindToText ConflictCherryPick = "cherry_pick"
conflictKindToText ConflictFixApply = "fix_apply"

conflictKindFromText :: Text -> ConflictKind
conflictKindFromText "fix_apply" = ConflictFixApply
conflictKindFromText _ = ConflictCherryPick

resolutionEncodingToText :: ResolutionEncoding -> Text
resolutionEncodingToText EncodingText = "text"
resolutionEncodingToText EncodingBase64 = "base64"
resolutionEncodingToText EncodingGitlink = "gitlink"

resolutionEncodingFromText :: Text -> ResolutionEncoding
resolutionEncodingFromText "base64" = EncodingBase64
resolutionEncodingFromText "gitlink" = EncodingGitlink
resolutionEncodingFromText _ = EncodingText

stepStatusToText :: StepStatus -> Text
stepStatusToText StepPending = "pending"
stepStatusToText StepCherryPicked = "cherry_picked"
stepStatusToText StepFixed = "fixed"
stepStatusToText StepVerified = "verified"
stepStatusToText StepSkipped = "skipped"

stepStatusFromText :: Text -> StepStatus
stepStatusFromText "cherry_picked" = StepCherryPicked
stepStatusFromText "fixed" = StepFixed
stepStatusFromText "verified" = StepVerified
stepStatusFromText "skipped" = StepSkipped
stepStatusFromText _ = StepPending

executionModeToText :: ExecutionMode -> Text
executionModeToText ExecutionExec = "exec"
executionModeToText ExecutionStep = "step"

executionModeFromText :: Text -> ExecutionMode
executionModeFromText "step" = ExecutionStep
executionModeFromText _ = ExecutionExec

executionStatusToText :: ExecutionStatus -> Text
executionStatusToText StatusPending = "pending"
executionStatusToText StatusInProgress = "in_progress"
executionStatusToText StatusConflict = "conflict"
executionStatusToText StatusCompleted = "completed"
executionStatusToText StatusFailed = "failed"
executionStatusToText StatusAborted = "aborted"

executionStatusFromText :: Text -> ExecutionStatus
executionStatusFromText "in_progress" = StatusInProgress
executionStatusFromText "conflict" = StatusConflict
executionStatusFromText "completed" = StatusCompleted
executionStatusFromText "failed" = StatusFailed
executionStatusFromText "aborted" = StatusAborted
executionStatusFromText _ = StatusPending

errorTypeToText :: ErrorType -> Text
errorTypeToText ErrorUnmappedFile = "unmapped_file"
errorTypeToText ErrorOutsideAncestry = "outside_ancestry"
errorTypeToText ErrorAmbiguousBranch = "ambiguous_branch"

errorTypeFromText :: Text -> ErrorType
errorTypeFromText "outside_ancestry" = ErrorOutsideAncestry
errorTypeFromText "ambiguous_branch" = ErrorAmbiguousBranch
errorTypeFromText _ = ErrorUnmappedFile
