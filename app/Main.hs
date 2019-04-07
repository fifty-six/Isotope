module Main where

import DataType
import Lib
import Options.Applicative
import Text.Pretty.Simple

data Command =
    CheckSignups         |
    GetSchedule          |
    GetEmergencyMessages |
    CheckActivityLocation {
        activity :: String
    } |
    SignupBlock {
        block    :: BlockInput,
        activity :: String
    }
    deriving (Show)

parseActivity :: Parser String
parseActivity = argument
        str (
            metavar "ACTIVITY"
            <> help "Name of activity"
        )

parseBlock :: Parser BlockInput
parseBlock = argument
        str (
            metavar "BLOCK"
            <> help "Block to signup for"
        )

parseCommand :: Parser Command
parseCommand = subparser $
    command
        "signups"
        (info
            (helper <*> pure CheckSignups)
            (fullDesc <> progDesc "List signups.")
        )
    <>
    command
        "location"
        (info
            (helper <*> fmap CheckActivityLocation parseActivity)
            (fullDesc <> progDesc "Get location of activity given name")
        )
    <>
    command
        "signup"
        (info
            (helper <*> liftA2 SignupBlock parseBlock parseActivity)
            (fullDesc <> progDesc "Signup for activity given block and name")
        )
    <>
    command
        "schedule"
        (info
            (helper <*> pure GetSchedule)
            (fullDesc <> progDesc "Display schedule for today or next available day.")
        )
    <>
    command
        "emergency"
        (info
            (helper <*> pure GetEmergencyMessages)
            (fullDesc <> progDesc "Display emergency status and message if there is any.")
        )

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)

main :: IO ()
main = do
    commandType <- showHelpOnErrorExecParser (info (helper <*> parseCommand)
                    (fullDesc <>
                    progDesc "Use isotope to manage Ion 8th period activities." <>
                    header "Isotope: Ion CLI"))

    case commandType of
        CheckSignups -> pPrint =<< fetchSignedUpInfo
        CheckActivityLocation activity_name -> pPrint =<< getActivity activity_name
        SignupBlock block_type activity_name -> pPrint =<< signup block_type activity_name
        GetSchedule -> pPrint =<< getSchedule
        GetEmergencyMessages -> pPrint =<< getEmergencyMessage