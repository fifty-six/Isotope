{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

module DataType where

import           Control.Exception
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Map           (Map)
import           Data.String
import           Data.Time.Calendar
import           GHC.Generics

-- | Strip prefix for usage in FromJSON.
removePrefix :: String      -- ^ Prefix to strip
             -> [String]    -- ^ Exceptions in which the prefix should not be stripped.
             -> String      -- ^ Field to strip prefix from
             -> String      -- ^ Field with stripped prefix.
removePrefix prefix exceptions field
    | field `elem` exceptions = field
    | otherwise = drop (length prefix) field

parseWithPrefix :: (Generic a, GFromJSON Zero (Rep a)) => String -> [String] -> Value -> Parser a
parseWithPrefix prefix exceptions = genericParseJSON
    defaultOptions {
         fieldLabelModifier = removePrefix prefix exceptions
    }

data BlockInput = A | B
    deriving (Show)

newtype InvalidBlockException =
    InvalidBlockException { input :: String }
    deriving Show

instance Exception InvalidBlockException

instance IsString BlockInput where
    fromString a = case a of
        "A" -> A
        "B" -> B
        _   -> throw (InvalidBlockException a)

data Blocks =
    Blocks { blocks_count    :: Integer
           , blocks_next     :: Maybe Blocks
           , blocks_previous :: Maybe Blocks
           , blocks_results  :: [Block]
           }
           deriving (Show, Generic)

instance FromJSON Blocks where
    parseJSON = parseWithPrefix "blocks_" []

data Block =
    Block { block_id     :: Integer
          , block_url    :: String
          , block_date   :: Day
          , block_letter :: String
          , block_locked :: Maybe Bool
          }
    deriving (Show, Generic)

instance FromJSON Block where
    parseJSON = parseWithPrefix "block_" ["block_letter"]

data Activities =
    Activities { activities_id         :: Integer
               , activities_activities :: Map Integer Activity
               } deriving (Show, Generic)

instance FromJSON Activities where
    parseJSON = parseWithPrefix "activities_" []

data Activity =
    Activity { aid   :: Integer
             , name  :: String
             , rooms :: [String]
             } deriving (Show, Generic)

-- FromJSON doesn't parse with a prefix here because
------ There are no name conflicts as this is the only non-prefixed record.
------ I don't want to implemenet ToJSON right now.
instance FromJSON Activity
instance ToJSON Activity

data SignupActivity =
    SignupActivity { signupActivity_title :: String
                   , signupActivity_id    :: Int
                   } deriving (Show, Generic)

instance FromJSON SignupActivity where
    parseJSON = parseWithPrefix "signupActivity_" []

data Signup =
    Signup { signup_id                 :: Integer
           , signup_block              :: Block
           , signup_activity           :: SignupActivity
           , signup_scheduled_activity :: Int
           , signup_user               :: Int
           } deriving (Show, Generic)

instance FromJSON Signup where
    parseJSON = parseWithPrefix "signup_" []

data ScheduleResponse =
    ScheduleResponse { scheduleResponse_count    :: Integer
                     , scheduleResponse_next     :: Maybe String
                     , scheduleResponse_previous :: Maybe String
                     , scheduleResponse_results  :: [ScheduleResult]
                     } deriving (Show, Generic)

instance FromJSON ScheduleResponse where
    parseJSON = parseWithPrefix "scheduleResponse_" []

data ScheduleResult =
    ScheduleResult { scheduleResult_date       :: Day
                   , scheduleResult_day_type   :: Schedule
                   } deriving (Show, Generic)

instance FromJSON ScheduleResult where
    parseJSON = parseWithPrefix "scheduleResult_" []

data Schedule =
    Schedule { schedule_name    :: String
             , schedule_special :: Bool
             , schedule_blocks  :: [ScheduleBlock]
             } deriving (Show, Generic)

instance FromJSON Schedule where
    parseJSON = parseWithPrefix "schedule_" []

type Timestamp = String

data ScheduleBlock =
    ScheduleBlock { scheduleBlock_order :: Integer
                  , scheduleBlock_name  :: String
                  , scheduleBlock_start :: Timestamp
                  , scheduleBlock_end   :: Timestamp
                  } deriving (Show, Generic)

instance FromJSON ScheduleBlock where
    parseJSON = parseWithPrefix "scheduleBlock_" []

data EmergencyMessage =
    EmergencyMessage { emergency_status  :: Bool
                     , emergency_message :: Maybe String
                     } deriving (Show, Generic)

instance FromJSON EmergencyMessage where
    parseJSON = parseWithPrefix "emergency_" []
