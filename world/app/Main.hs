{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Lens.TH (makeLenses)
import Text.Parsec
import Text.Parsec.ByteString
import qualified Data.ByteString as BS
import Data.List (groupBy, maximumBy, minimumBy)
import Data.Ord (comparing)
import Debug.Trace (trace)
import Text.Read (readMaybe)
import System.IO (hSetEncoding, stdout, utf8)

data DataRow = DataRow
  { _trackId :: String
  , _trackName :: String
  , _trackAlbumId :: String
  , _trackAlbumName :: String
  , _trackAlbumReleaseDate :: String
  , _trackDuration :: Int
  }

makeLenses ''DataRow

data Track = Track
  { _ident :: String
  , _name :: String
  , _duration :: Int
  }

makeLenses ''Track

data Album = Album
  { _albumId :: String
  , _albumName :: String
  , _albumReleaseDate :: String
  , _albumTracks :: [Track]
  }

makeLenses ''Album

csvParser :: Parser [String]
csvParser = many1 (Text.Parsec.noneOf ",\n") `sepBy` char ',' <* optional (char '\n')

parseDataRow :: Parser DataRow
parseDataRow = do
  fields <- csvParser
  let [trackId, trackName, _, _, trackAlbumId, trackAlbumName, trackAlbumReleaseDate, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, duration] = fields
  case readMaybe duration of
    Just durationValue -> return $ DataRow trackId trackName trackAlbumId trackAlbumName trackAlbumReleaseDate durationValue
    Nothing -> fail $ "Failed to parse duration: " ++ duration

parseFile :: FilePath -> IO [DataRow]
parseFile filePath = do
  content <- BS.readFile filePath
  case parse (header >> manyTill parseDataRow eof) filePath content of
    Left err -> error $ "Parse error: " ++ show err
    Right result -> return result

header :: Parser ()
header = csvParser >> optional (char '\n') >> return ()

toAlbums :: [DataRow] -> [Album]
toAlbums dataRows =
  dataRows
    & groupBy (\x y -> view trackAlbumId x == view trackAlbumId y)
    & fmap (\group -> Album
              { _albumId = view trackAlbumId (head group)
              , _albumName = view trackAlbumName (head group)
              , _albumReleaseDate = view trackAlbumReleaseDate (head group)
              , _albumTracks = group & traverse %~ (\row -> Track
                                                        { _ident = view trackId row
                                                        , _name = view trackName row
                                                        , _duration = view trackDuration row
                                                        })
              })

longestAlbum :: [Album] -> Album
longestAlbum = maximumBy (comparing (sum . map _duration . _albumTracks))

averageLengthAlbum :: [Album] -> Album
averageLengthAlbum = maximumBy (comparing (averageLength . map _duration . _albumTracks))
  where
    averageLength :: [Int] -> Double
    averageLength durations = fromIntegral (sum durations) / fromIntegral (length durations)

shortestAlbum :: [Album] -> Album
shortestAlbum = minimumBy (comparing (sum . map _duration . _albumTracks))

instance Show Album where
  show album =
    "Album Id: " ++ view albumId album ++ " \n " ++
    "Album Name: " ++ view albumName album ++ " \n " ++
    "Release Date: " ++ view albumReleaseDate album ++ " \n " ++
    "Tracks:\n" ++ concatMap showTrack (view albumTracks album) ++ " \n "

showTrack :: Track -> String
showTrack track =
  "  Track Id: " ++ view ident track ++ "\n" ++
  "  Track Name: " ++ view name track ++ "\n" ++
  "  Duration: " ++ show (view duration track) ++ " ms\n"

main :: IO ()
main = do
  hSetEncoding stdout utf8
  dataRows <- parseFile "spotify_songs.csv"
  let albums = toAlbums dataRows

  putStrLn "Longest Album:"
  putStr $ show $ longestAlbum albums

  putStrLn "\n Album with the Highest Average Song Length:"
  putStr $ show $ averageLengthAlbum albums

  putStrLn "\n Shortest Album:"
  putStr $ show $ shortestAlbum albums
