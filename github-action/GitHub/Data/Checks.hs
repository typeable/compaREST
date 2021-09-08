module GitHub.Data.Checks
  ( Check (..),
    CheckStatus (..),
    CheckConclusion (..),
    CheckOutput (..),
    CheckAnnotation (..),
    CheckAnnotationLevel (..),
    CheckImage (..),
    CheckAction (..),
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import GitHub
import GitHub.Internal.Prelude (UTCTime)

data Check = Check
  { checkName :: !(Name Check)
  , checkSha :: !(Name Commit)
  , checkDetailsURL :: !(Maybe URL)
  , checkExternalId :: !(Maybe (Id Check))
  , checkStatus :: !(Maybe CheckStatus)
  , checkStartedAt :: !(Maybe UTCTime)
  , checkConclusion :: !(Maybe CheckConclusion)
  , checkCompletedAt :: !(Maybe UTCTime)
  , checkOutput :: !(Maybe CheckOutput)
  , checkActions :: !(Maybe (Vector CheckAction))
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON Check where
  toJSON (Check n sha durl eid s sa c ca o a) =
    object'
      [ "name" .= n
      , "head_sha" .= sha
      , "details_url" .= durl
      , "external_id" .= eid
      , "status" .= s
      , "started_at" .= sa
      , "conclusion" .= c
      , "completed_at" .= ca
      , "output" .= o
      , "actions" .= a
      ]

data CheckStatus
  = CheckQueued
  | CheckInProgress
  | CheckCompleted
  deriving stock (Show, Enum, Bounded, Eq, Ord, Generic)

instance ToJSON CheckStatus where
  toJSON CheckQueued = String "queued"
  toJSON CheckInProgress = String "in_progress"
  toJSON CheckCompleted = String "completed"

data CheckConclusion
  = CheckActionRequired
  | CheckCancelled
  | CheckFailure
  | CheckNeutral
  | CheckSuccess
  | CheckSkipped
  | CheckStale
  | CheckTimedOut
  deriving stock (Show, Enum, Bounded, Eq, Ord, Generic)

instance ToJSON CheckConclusion where
  toJSON CheckActionRequired = String "action_required"
  toJSON CheckCancelled = String "cancelled"
  toJSON CheckFailure = String "failure"
  toJSON CheckNeutral = String "neutral"
  toJSON CheckSuccess = String "success"
  toJSON CheckSkipped = String "skipped"
  toJSON CheckStale = String "stale"
  toJSON CheckTimedOut = String "timed_out"

data CheckOutput = CheckOutput
  { checkTitle :: !Text
  , checkSummary :: !Text
  , checkText :: !(Maybe Text)
  , checkAnnotations :: !(Maybe (Vector CheckAnnotation))
  , checkImages :: !(Maybe (Vector CheckImage))
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON CheckOutput where
  toJSON (CheckOutput t s txt a i) =
    object'
      [ "title" .= t
      , "summary" .= s
      , "text" .= txt
      , "annotations" .= a
      , "images" .= i
      ]

data CheckAnnotation = CheckAnnotation
  { checkPath :: !Text
  , checkStartLine :: !Int
  , checkEndLine :: !Int
  , checkStartColumn :: !(Maybe Int)
  , checkEndColumn :: !(Maybe Int)
  , checkAnnotationLevel :: !CheckAnnotationLevel
  , checkMessage :: !Text
  , checkTitle :: !(Maybe Text)
  , checkRawDetails :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON CheckAnnotation where
  toJSON (CheckAnnotation p sl el sc ec al m t rd) =
    object'
      [ "path" .= p
      , "start_line" .= sl
      , "end_line" .= el
      , "start_column" .= sc
      , "end_column" .= ec
      , "annotation_level" .= al
      , "message" .= m
      , "title" .= t
      , "raw_details" .= rd
      ]

data CheckAnnotationLevel
  = NoticeAnnotation
  | WarningAnnotation
  | FailureAnnotation
  deriving stock (Show, Enum, Bounded, Eq, Ord, Generic)

instance ToJSON CheckAnnotationLevel where
  toJSON NoticeAnnotation = String "notice"
  toJSON WarningAnnotation = String "warning"
  toJSON FailureAnnotation = String "failure"

data CheckImage = CheckImage
  { checkImageAlt :: !Text
  , checkImageURL :: !URL
  , checkImageCaption :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON CheckImage where
  toJSON (CheckImage a url c) =
    object'
      [ "alt" .= a
      , "image_url" .= url
      , "caption" .= c
      ]

data CheckAction = CheckAction
  { checkActionLabel :: !Text
  , checkActionDescription :: !Text
  , checkActionIdentifier :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON CheckAction where
  toJSON (CheckAction l d i) =
    object'
      [ "label" .= l
      , "description" .= d
      , "identifier" .= i
      ]

object' :: [Pair] -> Value
object' = object . filter notNull
  where
    notNull (_, Null) = False
    notNull (_, _) = True
