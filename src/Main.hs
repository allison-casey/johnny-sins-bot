{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DuplicateRecordFields, OverloadedStrings  #-}
module Main where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Discord                       as DSC
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as B

import           Control.Exception              ( finally )
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Data.Char                      ( toLower )
import           Data.Monoid                    ( (<>) )
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                , decode
                                                )
import           Data.List                      ( find )
import           Data.Maybe                     ( Maybe )
import           System.Random                  ( randomRIO
                                                , randomIO
                                                )

data BotTrigger = BotTrigger
  { keywords :: [T.Text]
  , probability :: Double
  , responses :: [T.Text] } deriving (Show, Generic, ToJSON, FromJSON)

data BotConfig = BotConfig
  { title :: T.Text
  , description :: T.Text
  , probability :: Double
  , triggers :: [BotTrigger] } deriving (Show, Generic, ToJSON, FromJSON)

readConfig :: IO (Maybe BotConfig)
readConfig = do
  rawJSON <- B.readFile "./config.json"
  let config = decode rawJSON :: Maybe BotConfig
  return config

bootstrapBot :: BotConfig -> IO ()
bootstrapBot conf = do
  token   <- T.strip <$> TIO.readFile "./auth-token.secret"
  discord <- DSC.loginRestGateway (DSC.Auth token)
  finally (loopingMain conf discord) (DSC.stopDiscord discord)

-- | Kicks of the primary event loop for the bot, initializing all necessary
-- connections and authentication tokens.
main :: IO ()
main = do
  config <- readConfig
  case config of
    Nothing   -> putStrLn "[ERROR] Failed to parse config"
    Just conf -> bootstrapBot conf

processMessage conf disc message = case responsibleTrigger of
  Nothing      -> return ()
  Just trigger -> do
    let masterProb  = probability (conf :: BotConfig)
    let triggerProb = probability (trigger :: BotTrigger)
    shouldBotRespond <- shouldRespond $ masterProb * triggerProb
    when shouldBotRespond $ do
      response <- generateResponse trigger
      sendChannelMessage channelID response
 where
  messageText        = DSC.messageText message
  channelID          = DSC.messageChannel message
  responsibleTrigger = getResponsibleTrigger (triggers conf) messageText
  sendChannelMessage channelID message = do
    _ <- DSC.restCall disc (DSC.CreateMessage channelID message)
    return ()
-- | Primary event loop for bot actions. Will recieve all messages in a server
-- and respond with a quote from your boi Johnny Sins
loopingMain :: BotConfig -> (DSC.RestChan, DSC.Gateway, z) -> IO ()
loopingMain conf disc = do
  e <- DSC.nextEvent disc
  case e of
    Left  er                    -> putStrLn ("Event error: " <> show er)
    Right (DSC.MessageCreate m) -> finally
      (unless (fromBot m) $ processMessage conf disc m)
      (loopingMain conf disc)
    _ -> loopingMain conf disc

-- | Checks if a message is from a bot
fromBot :: DSC.Message -> Bool
fromBot m = case DSC.messageAuthor m of
  Right u          -> DSC.userIsBot u
  Left  _webhookid -> True

-- | Finds the trigger responsible for handling the given message
getResponsibleTrigger :: [BotTrigger] -> T.Text -> Maybe BotTrigger
getResponsibleTrigger triggers message = find checkTrigger triggers
 where
  checkTrigger trigger =
    any (`caseInsensitiveIsInfix` message) (keywords trigger)

-- | A case insensitive version of isInfixOf
caseInsensitiveIsInfix :: T.Text -> T.Text -> Bool
caseInsensitiveIsInfix key str = T.isInfixOf key $ T.toLower str

-- | Checks if the message contains any of the key words defined in the
-- bot triggers list
isKeyword :: [BotTrigger] -> T.Text -> Bool
isKeyword triggers message = any checkTriggerKeywords triggers
 where
  caseInsensitiveIsInfix key str = T.isInfixOf key $ T.toLower str
  checkTriggerKeywords trigger =
    any (`caseInsensitiveIsInfix` message) (keywords trigger)

-- | Pulls a random response from the trigger config
generateResponse :: BotTrigger -> IO T.Text
generateResponse trigger = (responsesList !!)
  <$> randomRIO (0, length responsesList - 1)
  where responsesList = responses trigger

-- | Based on a given probability. randomly determine if the bot
-- should respond
shouldRespond :: Double -> IO Bool
shouldRespond prob = do
  chance <- randomIO :: IO Double
  return $ chance <= prob
