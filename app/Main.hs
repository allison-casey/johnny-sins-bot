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
import System.Environment
import System.Exit
-- | Kicks of the primary event loop for the bot, initializing all necessary
-- connections and authentication tokens.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [configPath, tokenPath] -> runBot configPath tokenPath
    _ -> putStrLn "[ERROR]: Invalid arguments -> johnny-sins-bot CONFIG_PATH TOKEN_PATH"
  -- runBot


runBot :: String -> String -> IO ()
runBot configPath tokenPath = do
  config <- readConfig configPath
  token   <- T.strip <$> TIO.readFile tokenPath
  case config of
    Nothing   -> putStrLn "[ERROR] Failed to parse config"
    Just conf -> bootstrapBot conf token

bootstrapBot :: BotConfig -> T.Text -> IO ()
bootstrapBot conf token = do
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
