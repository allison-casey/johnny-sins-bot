{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DuplicateRecordFields  #-}
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

-- | Kicks of the primary event loop for the bot, initializing all necessary
-- connections and authentication tokens.
main :: IO ()
main = do
  rawJSON <- B.readFile "./config.json"
  let config = decode rawJSON :: Maybe BotConfig
  case config of
    Nothing   -> putStrLn "[ERROR] Failed to parse config"
    Just conf -> do
      token   <- T.strip <$> TIO.readFile "./auth-token.secret"
      discord <- DSC.loginRestGateway (DSC.Auth token)
      finally (loopingMain conf discord) (DSC.stopDiscord discord)

-- | Primary event loop for bot actions. Will recieve all messages in a server
-- and respond with a quote from your boi Johnny Sins
loopingMain :: BotConfig -> (DSC.RestChan, DSC.Gateway, z) -> IO ()
loopingMain conf dis = do
  e <- DSC.nextEvent dis
  case e of
    Left  er                    -> putStrLn ("Event error: " <> show er)
    Right (DSC.MessageCreate m) -> do
      unless (fromBot m) $ do
        let message                   = DSC.messageText m
        let channelID                 = DSC.messageChannel m
        let responsibleTrigger = getResponsibleTrigger triggersList message
        let masterResponseProbability = probability (conf :: BotConfig)
        case responsibleTrigger of
          Nothing      -> return ()
          Just trigger -> do
            let triggerResponseProbability =
                  probability (trigger :: BotTrigger)
            shouldBotRespond <- shouldRespond
              (masterResponseProbability * triggerResponseProbability)
            print $ masterResponseProbability * triggerResponseProbability
            print shouldBotRespond
            when shouldBotRespond $ do
              response <- generateResponse trigger
              sendChannelMessage channelID response

      loopingMain conf dis
    _ -> loopingMain conf dis
 where
  triggersList = triggers conf
  sendChannelMessage channelID message = do
    _ <- DSC.restCall dis (DSC.CreateMessage channelID message)
    return ()

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
