{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Text.Printf (printf)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import System.Directory
import System.Environment
import System.FilePath
import System.IO

newtype FileSize = FileSize Integer deriving (Show, Eq, Ord, Num)

data Condition = Condition
  {
    totalSpace :: FileSize
  } deriving (Show)

data Config = Config
  {
    readDepth :: Int,
    needFormat :: Bool,
    verbose :: Bool
  }

newtype DiskUsageMonad diskUsageMonad = DiskUsageMonad
  {
    unwrapDiskUsageMonad :: ReaderT Config (ExceptT String (WriterT String (StateT Condition IO))) diskUsageMonad
  }
  deriving ( Functor,
             Applicative,
             Monad,
             MonadIO,
             MonadReader Config,
             MonadError String,
             MonadWriter String,
             MonadState Condition
           )

runDiskUsageMonad :: DiskUsageMonad a -> Config -> Condition -> IO (Either String a, String, Condition)
runDiskUsageMonad action config state = do
  ((result, logOutput), finalState) <- runStateT (runWriterT (runExceptT (runReaderT (unwrapDiskUsageMonad action) config))) state
  return (result, logOutput, finalState)

getFilesSize :: FilePath -> DiskUsageMonad FileSize
getFilesSize path = do
  size <- liftIO $ withFile path ReadMode hFileSize
  return $ FileSize (fromIntegral size)

diskUsage :: FilePath -> DiskUsageMonad ()
diskUsage path = do
  config <- ask
  isDir <- liftIO $ doesDirectoryExist path
  when (verbose config) $
    liftIO $ putStrLn $ "Путь: " ++ path
  if isDir
    then do
      contents <- liftIO $ listDirectory path
      forM_ contents $ \name -> do
        let newPath = path </> name
        when (verbose config) $
          liftIO $ putStrLn $ "  Поддиректория: " ++ newPath
        isDir <- liftIO $ doesDirectoryExist newPath
        if isDir
          then do
            diskUsage newPath
            modify (\s -> s {totalSpace = totalSpace s + 1})
          else do
            fileSize <- getFilesSize newPath
            modify (\s -> s {totalSpace = totalSpace s + fileSize})
    else throwError $ path ++ " не является директорией"

parseArgs :: [String] -> Either String (Config, [FilePath])
parseArgs args = loop (Config 0 False False, []) args
  where
    loop (config, dirs) [] = Right (config, reverse dirs)
    loop (config, dirs) (arg:rest) =
      case arg of
        "-d" -> loop (config {readDepth = read (head rest)}, dirs) (tail rest)
        "-s" -> loop (config {readDepth = 0}, dirs) rest
        "-h" -> loop (config {needFormat = True}, dirs) rest
        "-v" -> loop (config {verbose = True}, dirs) rest
        dir -> loop (config, dir : dirs) rest

formatSize :: FileSize -> String
formatSize (FileSize bytes)
  | bytes < k           = show bytes ++ " Б"
  | bytes < m           = printf "%.2f КБ" (fromIntegral bytes / k' :: Double)
  | bytes < g           = printf "%.2f МБ" (fromIntegral bytes / m' :: Double)
  | bytes < t           = printf "%.2f ГБ" (fromIntegral bytes / g' :: Double)
  | otherwise           = printf "%.2f ТБ" (fromIntegral bytes / t' :: Double)
  where
    k = 1024
    m = k * k
    g = m * k
    t = g * k
    k' = fromIntegral k
    m' = fromIntegral m
    g' = fromIntegral g
    t' = fromIntegral t

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> putStrLn err
    Right (config, dirs) -> do
      (_, logOutput, finalState) <- runDiskUsageMonad (mapM_ diskUsage dirs) config (Condition 0)
      putStrLn logOutput
      when (needFormat config) $
        putStrLn $ "Занимаемое место: " ++ formatSize (totalSpace finalState)

      unless (needFormat config) $
        print finalState