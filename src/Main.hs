module Main where

import System.Exit
import System.Environment
import System.Directory
import Data.Time.LocalTime
import Data.Time
import Data.Function
import Control.Monad

data Pause
  = AngebrochenePause ZonedTime
  | BeendetePause ZonedTime ZonedTime
  deriving (Show, Read)

data Arbeitstag
  = Angebrochen ZonedTime [Pause]
  | Beendet ZonedTime ZonedTime [Pause]
  deriving (Show, Read)

data Stempelkarte = Stempelkarte [Arbeitstag]
  deriving (Show, Read)

type ErrorMessage = (String, Int)

sameDay :: ZonedTime -> ZonedTime -> Bool
sameDay = (==) `on` (localDay . zonedTimeToLocalTime)

y_or_n_p :: String -> String -> IO Bool
y_or_n_p prompt err = do
  putStrLn prompt
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _   -> do
      putStrLn err
      y_or_n_p prompt err

now :: IO ZonedTime
now = getCurrentTime >>= utcToLocalZonedTime

beginDay :: Stempelkarte -> IO (Either ErrorMessage Stempelkarte)
beginDay (Stempelkarte (Angebrochen _ _ : _)) = pure $ Left ("Es wurde bereits ein Tag begonnen.", 1)
beginDay (Stempelkarte []) = do
  t <- now
  pure $ Right $ Stempelkarte (Angebrochen t []:[])
beginDay (Stempelkarte days@(Beendet begin end pauses:days')) = do
  t <- now
  if t `sameDay` end
    then do
      reopen <- y_or_n_p
        "Dieser Tag wurde schon beendet; möchtest du ihn forführen? [y/n]"
        "Bitte gib nur „y“ oder „n“ ein!"
      if reopen
        then pure $ Right $ Stempelkarte (Angebrochen begin pauses:days')
        else pure $ Left ("Der Tag wurde bereits beendet.", 2)
    else do
      pure $ Right $ Stempelkarte (Angebrochen t [] : days)

endDay :: Stempelkarte -> IO (Either ErrorMessage Stempelkarte)
endDay (Stempelkarte []) = pure $ Left ("Es wurde noch kein Tag begonnen.", 8)
endDay (Stempelkarte (Beendet _ _ _: _)) = pure $ Left ("Dieser Tag wurde bereits beendet.", 3)
endDay (Stempelkarte (Angebrochen begin pauses:days)) = do
  t <- now
  pure $ Right $ Stempelkarte (Beendet begin t pauses:days)

beginPause :: Stempelkarte -> IO (Either ErrorMessage Stempelkarte)
beginPause (Stempelkarte []) = pure $ Left ("Es wurde noch kein Tag begonnen", 10)
beginPause (Stempelkarte (Beendet _ _ _ : _)) = pure $ Left ("Dieser Tag wurde bereits beendet.", 4)
beginPause (Stempelkarte (Angebrochen begin pauses : days)) = do
  pauses' <- beginPause' pauses
  case pauses' of
    Left err -> pure $ Left err
    Right pauses'' -> pure $ Right $ Stempelkarte (Angebrochen begin pauses'' : days)

beginPause' :: [Pause] -> IO (Either ErrorMessage [Pause])
beginPause' (AngebrochenePause _ : _) = pure $ Left ("Die Pause wurde bereits begonnen.", 5)
beginPause' [] = do
  t <- now
  pure $ Right [AngebrochenePause t]
beginPause' pauses@(BeendetePause _ _ : _) = do
  t <- now
  pure $ Right $ AngebrochenePause t : pauses

endPause :: Stempelkarte -> IO (Either ErrorMessage Stempelkarte)
endPause (Stempelkarte []) = pure $ Left ("Es wurde noch kein Tag begonnen.", 9)
endPause (Stempelkarte (Beendet _ _ _ : _)) = pure $ Left ("Dieser Tag wurde bereits beendet.", 6)
endPause (Stempelkarte (Angebrochen begin pauses : days)) = do
  pauses' <- endPause' pauses
  case pauses' of
    Left err -> pure $ Left err
    Right pauses'' -> pure $ Right $ Stempelkarte (Angebrochen begin pauses'' : days)

endPause' :: [Pause] -> IO (Either ErrorMessage [Pause])
endPause' [] = pure $ Left ("Es gibt keine Pausen.", 11)
endPause' (BeendetePause _ _ : _) = pure $ Left ("Die Pause wurde bereits beendet.", 6)
endPause' (AngebrochenePause begin : pauses) = do
  t <- now
  pure $ Right $ BeendetePause begin t : pauses

withStempelkarte :: FilePath -> (Stempelkarte -> IO (Either ErrorMessage Stempelkarte)) -> IO ()
withStempelkarte fp f = do
  e <- doesFileExist fp
  sk <- if e then read <$> readFile fp
             else pure $ Stempelkarte []
  sk' <- f sk
  case sk' of
    Left (err, code) -> do
      putStrLn err
      exitWith $ ExitFailure code
    Right sk'' -> do
      writeFile fp (show sk'')
      exitWith ExitSuccess

printStempelkarte :: Stempelkarte -> IO (Either ErrorMessage Stempelkarte)
printStempelkarte sk@(Stempelkarte ps) = do
  forM_ (reverse ps) $ \p -> case p of
    Angebrochen t pauses -> do
      putStrLn $ "ab " ++ show t
      forM_ (reverse pauses) $ \pause -> do
        putStrLn $ "  " ++ show pause
    Beendet t1 t2 pauses -> do
      putStrLn $ "von " ++ show t1 ++ " bis " ++ show t2
      forM_ (reverse pauses) $ \pause -> do
        putStrLn $ "  " ++ show pause
  pure $ Right $ sk

main' :: String -> String -> IO ()
main' fp "beginn"       = withStempelkarte fp beginDay
main' fp "ende"         = withStempelkarte fp endDay
main' fp "pause_beginn" = withStempelkarte fp beginPause
main' fp "pause_ende"   = withStempelkarte fp endPause
main' fp "auflisten"    = withStempelkarte fp printStempelkarte
main' _ others          = do
  putStrLn $ "„" ++ others ++ "“ ist kein Kommando! Nutze „beginn“, „ende“, „pause_beginn“ oder „pause_ende“."
  exitWith $ ExitFailure 7

main :: IO ()
main = do
  arg:_ <- getArgs
  main' "stempelkarte" arg
