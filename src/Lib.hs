{-# LANGUAGE OverloadedStrings        #-}

module Lib
    ( fetchSignedUpInfo
    , getActivity
    , signup
    , getSchedule
    , getEmergencyMessage
    ) where

import           Control.Concurrent.Async
import           Control.Lens.Operators
import           Data.Aeson
import qualified Data.Map                      as Map
import           Data.List
import           Data.Time.Calendar
import           Data.Time.Clock
import           DataType
import           Network.Wreq
import qualified Text.Fuzzy as Fuzzy

-- | Return current day.
today :: IO Day
today = utctDay <$> getCurrentTime

-- | URL for retrival of blocks.
blocksUrl :: IO String
blocksUrl = (<>) "https://ion.tjhsst.edu/api/blocks?format=json&start_date=" . show <$> today

-- | URL for retrival of signed up activites and for signing up for activities.
signupUrl :: String
signupUrl = "https://ion.tjhsst.edu/api/signups/user/?format=json"

-- | URL for retrival of schedule.
scheduleUrl :: String
scheduleUrl = "https://ion.tjhsst.edu/api/schedule?format=json"

-- | URL for retrival of emergency announcements.
emergencyUrl :: String
emergencyUrl = "https://ion.tjhsst.edu/api/emerg?format=json"

-- | Get response body of GET request with basicAuth.
getResponseBody :: FromJSON a => Credentials -> String -> IO a
getResponseBody cred url = (^. responseBody) <$> (asJSON =<< getWith opts url)
    where opts = defaults & auth ?~ basicAuth (cred_un cred) (cred_pass cred)

-- | Fetched signed up activities for user.
fetchSignedUp :: Credentials -> IO [Integer]
fetchSignedUp cred = do
    day <- today
    signups <- getResponseBody cred signupUrl :: IO [Signup]
    let minDay = minimum . filter (>= 0) . map (deltaDay day) $ signups
    return . map (signupActivity_id . signup_activity) . filter ((== minDay) . deltaDay day) $ signups

-- | Fetch signed up activities for user and then pair them with respective rooms.
fetchSignedUpInfo :: Credentials -> IO [String]
fetchSignedUpInfo cred = do
    concurrentTuple <- concurrently (fetchSignedUp cred) (fetchActivities cred)
    let (signedUp, activities) = concurrentTuple
    return (show . findActivityById activities <$> signedUp)

-- | Find the difference in days from given day and signup block day.
deltaDay :: Day -> Signup -> Integer
deltaDay day x = diffDays (block_date $ signup_block x) day

-- | Fetch list of blocks after today.
fetchBlocks :: Credentials -> IO [Block]
fetchBlocks cred = sortOn block_date . blocks_results <$> (getResponseBody cred =<< blocksUrl)

-- | Given a block, get respective activities for block.
fetchBlockActivities :: Credentials -> Block -> IO [Activity]
fetchBlockActivities cred block = Map.foldr (:) [] . activities_activities <$> getResponseBody cred (block_url block)

-- | Fetch activities for one day (2 blocks [A and B])
fetchActivities :: Credentials -> IO [Activity]
fetchActivities cred = fmap concat (mapConcurrently (fetchBlockActivities cred) =<< take 2 <$> fetchBlocks cred)

-- | Signup user for activity on given block. Return name of found activity (fuzzy matching)
signup :: Credentials -> BlockInput -> String -> IO String
signup cred blockType activity = do
    blocks <- fetchBlocks cred
    let block = case blockType of
            A -> head blocks
            B -> blocks !! 1
    activities <- fetchBlockActivities cred block
    let found = findActivityFuzzy activities activity
    _ <- postWith opts signupUrl ["block" := block_id block, "activity" := aid found]
    return $ "Signed up for " ++ name found
    where opts = defaults & auth ?~ basicAuth (cred_un cred) (cred_pass cred)

-- | Given fuzzy pattern for activity, return best matching activity (if any)
getActivity :: Credentials -> String -> IO Activity
getActivity cred = (??) (findActivityFuzzy <$> fetchActivities cred)

findActivityById :: [Activity] -> Integer -> Activity
findActivityById activities activityId = head $ filter ((==) activityId . aid) activities

-- | Given list of activities and fuzzy pattern for activity, return best matching activity (if any)
findActivityFuzzy :: [Activity] -> String -> Activity
findActivityFuzzy activities activity = Fuzzy.original $ head $
    Fuzzy.filter
    activity      -- ^ Pattern
    activities    -- ^ List
    mempty        -- ^ Append after
    mempty        -- ^ Append before
    name          -- ^ Map to text function
    False         -- ^ Case-insensitive

-- | Get schedule for current or next day if current day is not a school day.
getSchedule :: Credentials -> IO Schedule
getSchedule cred = scheduleResult_day_type . head . scheduleResponse_results <$> getResponseBody cred scheduleUrl

-- | Get emergency message if there is any.
getEmergencyMessage :: Credentials -> IO EmergencyMessage
getEmergencyMessage cred = getResponseBody cred emergencyUrl