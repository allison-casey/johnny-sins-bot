module Main where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Discord                       as DSC
import           Bot.Utils                      ( BotConfig
                                                , BotTrigger
                                                , fromBot
                                                )
import           Bot.IO                         ( processMessage
                                                , readConfig
                                                )
import           Control.Exception              ( finally )
import           Control.Monad                  ( unless
                                                , when
                                                )

-- | Kicks of the primary event loop for the bot, initializing all necessary
-- connections and authentication tokens.
main :: IO ()
main = runBot

runBot :: IO ()
runBot = do
  config <- readConfig "./config.json"
  case config of
    Nothing   -> putStrLn "[ERROR] Failed to parse config"
    Just conf -> bootstrapBot conf

bootstrapBot :: BotConfig -> IO ()
bootstrapBot conf = do
  token   <- T.strip <$> TIO.readFile "./auth-token.secret"
  discord <- DSC.loginRestGateway (DSC.Auth token)
  finally (loopingMain conf discord) (DSC.stopDiscord discord)


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
