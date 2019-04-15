{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}

module Main where

import Control.Exception
import Control.Lens.Operators
import Data.ByteString (ByteString)
import Data.Configurator
import Data.String
import DataType
import Lib
import Options.Applicative
import System.Directory
import Text.Pretty.Simple

data NoConfigException = NoConfigException

instance Show NoConfigException where
    show ex = "Config file doesn't exist."

instance Exception NoConfigException

data Options = 
    Options { configFile :: Maybe FilePath
            , subcommand :: Command
            }
            deriving (Show)

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

parseConfig :: Parser (Maybe FilePath)
parseConfig = 
        optional $ strOption (
            long "config-file"
            <> short 'c'
            <> help "Filepath of config"
            <> showDefault
        )

parseCommand :: Parser Command
parseCommand = subparser $
    command
        "signups"
        (info
            (helper ?? CheckSignups)
            (fullDesc <> progDesc "List signups.")
        )
    <>
    command
        "location"
        (info
            (helper <*> (CheckActivityLocation <$> parseActivity))
            (fullDesc <> progDesc "Get location of activity given name")
        )
    <>
    command
        "signup"
        (info
            (helper <*> (SignupBlock <$> parseBlock <*> parseActivity))
            (fullDesc <> progDesc "Signup for activity given block and name")
        )
    <>
    command
        "schedule"
        (info
            (helper ?? GetSchedule)
            (fullDesc <> progDesc "Display schedule for today or next available day.")
        )
    <>
    command
        "emergency"
        (info
            (helper ?? GetEmergencyMessages)
            (fullDesc <> progDesc "Display emergency status and message if there is any.")
        )

parseOptions = Options <$> parseConfig <*> parseCommand

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)

guard :: Exception a => Bool -> a -> IO ()
guard bool ex = if bool then return () else throw ex

main :: IO ()
main = do
    options <- showHelpOnErrorExecParser (info (helper <*> parseOptions)
                    (fullDesc <>
                    progDesc "Use isotope to manage Ion 8th period activities." <>
                    header "Isotope: Ion CLI"))

    let commandType = subcommand options

    configPath <- case configFile options of
        Nothing -> getXdgDirectory XdgConfig "isotope/config"
        Just a  -> return $ fromString a

    configExists <- doesFileExist configPath

    guard configExists NoConfigException

    config <- load [ Required configPath ]
    un     <- require config "username" :: IO ByteString
    pass   <- require config "password" :: IO ByteString

    let cred = Credentials { cred_un = un, cred_pass = pass }

    case commandType of
        CheckSignups -> pPrint =<< fetchSignedUpInfo cred
        CheckActivityLocation activity_name -> pPrint =<< getActivity cred activity_name
        SignupBlock block_type activity_name -> pPrint =<< signup cred block_type activity_name
        GetSchedule -> pPrint =<< getSchedule cred
        GetEmergencyMessages -> pPrint =<< getEmergencyMessage cred