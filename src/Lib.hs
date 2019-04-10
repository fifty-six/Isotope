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
import           Credentials
import           Data.Aeson
import qualified Data.Map                      as Map
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
getResponseBody :: FromJSON a => String -> IO a
getResponseBody url = (^. responseBody) <$> (asJSON =<< getWith opts url)
    where opts = defaults & auth ?~ basicAuth username pass

-- | Fetched signed up activities for user.
fetchSignedUp :: IO [Integer]
fetchSignedUp = do
    day <- today
    signups <- getResponseBody signupUrl :: IO [Signup]
    let minDay = minimum . filter (>= 0) . map (deltaDay day) $ signups
    return . map (signupActivity_id . signup_activity) . filter ((== minDay) . deltaDay day) $ signups

-- | Fetch signed up activities for user and then pair them with respective rooms.
fetchSignedUpInfo :: IO [String]
fetchSignedUpInfo = do
    concurrentTuple <- concurrently fetchSignedUp fetchActivities
    let (signedUp, activities) = concurrentTuple
    return (show . findActivityById activities <$> signedUp)

-- | Find the difference in days from given day and signup block day.
deltaDay :: Day -> Signup -> Integer
deltaDay day x = diffDays (block_date $ signup_block x) day

-- | Fetch list of blocks after today.
fetchBlocks :: IO [Block]
fetchBlocks = blocks_results <$> (getResponseBody =<< blocksUrl)

-- | Given a block, get respective activities for block.
fetchBlockActivities :: Block -> IO [Activity]
fetchBlockActivities block = Map.foldr (:) [] . activities_activities <$> getResponseBody (block_url block)

-- | Fetch activities for one day (2 blocks [A and B])
fetchActivities :: IO [Activity]
fetchActivities = fmap concat (mapConcurrently fetchBlockActivities =<< take 2 <$> fetchBlocks)

-- | Signup user for activity on given block. Return name of found activity (fuzzy matching)
signup :: BlockInput -> String -> IO String
signup blockType activity = do
    blocks <- fetchBlocks
    let block = case blockType of
            A -> head blocks
            B -> blocks !! 1
    activities <- fetchBlockActivities block
    let found = findActivityFuzzy activities activity
    _ <- postWith opts signupUrl ["block" := block_id block, "activity" := aid found]
    return $ "Signed up for " ++ name found
    where opts = defaults & auth ?~ basicAuth username pass

-- | Given fuzzy pattern for activity, return best matching activity (if any)
getActivity :: String -> IO Activity
getActivity = (??) (findActivityFuzzy <$> fetchActivities)

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
getSchedule :: IO Schedule
getSchedule = scheduleResult_day_type . head . scheduleResponse_results <$> getResponseBody scheduleUrl

-- | Get emergency message if there is any.
getEmergencyMessage :: IO EmergencyMessage
getEmergencyMessage = getResponseBody emergencyUrl