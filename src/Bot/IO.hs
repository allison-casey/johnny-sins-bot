{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}
module Bot.IO where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Discord                       as DSC
import qualified Data.ByteString.Lazy          as B

import           Control.Monad                  ( when )
import           Data.Aeson                     ( decode )
import           Data.Maybe                     ( Maybe )
import           System.Random                  ( randomRIO
                                                , randomIO
                                                )
import           Bot.Utils

debug :: Bool
debug = False

processMessage
  :: BotConfig -> (DSC.RestChan, DSC.Gateway, z) -> DSC.Message -> IO ()
processMessage conf disc message = case responsibleTrigger of
  Nothing      -> return ()
  Just trigger -> do
    let masterProb  = probability (conf :: BotConfig)
    let triggerProb = probability (trigger :: BotTrigger)
    shouldBotRespond <- shouldRespond $ masterProb * triggerProb
    when shouldBotRespond $ do
      response <- if debug
        then generateDebugResponse trigger (masterProb * triggerProb)
        else generateResponse trigger
      sendChannelMessage channelID response
 where
  messageText        = DSC.messageText message
  channelID          = DSC.messageChannel message
  responsibleTrigger = getResponsibleTrigger (triggers conf) messageText
  sendChannelMessage channelID message = do
    _ <- DSC.restCall disc (DSC.CreateMessage channelID message)
    return ()


-- | Pulls a random response from the trigger config
generateResponse :: BotTrigger -> IO T.Text
generateResponse trigger = (responsesList !!)
  <$> randomRIO (0, length responsesList - 1)
  where responsesList = responses trigger

generateDebugResponse :: BotTrigger -> Double -> IO T.Text
generateDebugResponse trigger responseProb = do
  response <- generateResponse trigger
  return $ T.intercalate
    "\n"
    [ response
    , "```CSS\n"
    , matchedOn
    , responseProbability
    , possibleResponses
    , "```"
    ]
 where
  matchedOn = "Matched On: " <> T.intercalate ", " (keywords trigger)
  responseProbability =
    "Response Probability: " <> (T.pack . show) responseProb
  possibleResponses =
    "Possible Responses: \n\t\t" <> T.intercalate "\n\t\t" (responses trigger)

-- | Based on a given probability. randomly determine if the bot
-- should respond
shouldRespond :: Double -> IO Bool
shouldRespond prob = do
  chance <- randomIO :: IO Double
  return $ chance <= prob

readConfig :: String -> IO (Maybe BotConfig)
readConfig path = do
  rawJSON <- B.readFile path
  let config = decode rawJSON :: Maybe BotConfig
  return config
