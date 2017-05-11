{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Teleport where

import Control.Monad
import Data.Maybe
import Data.List
import Prelude hiding (FilePath)
import qualified Data.Text as T
import Data.Text.Encoding
import Options.Applicative
import Data.Monoid
import Filesystem.Path.CurrentOS as P
import qualified Turtle
import Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified System.Console.ANSI as ANSI
import Filesystem as P
import System.Environment 
import Paths_teleport
import Data.Version
import Data.Default
import Control.Lens hiding (argument)
import Data.Composition
import GHC.Generics

-- | options for 'warp list'
data DisplayOptions = DisplayOptions deriving (Show)

-- | options for 'warp add'
data AddOptions = AddOptions { folderPath :: Maybe String,
                               addname    :: String }

-- | options for 'warp remove'
newtype RemoveOptions = RemoveOptions { removename :: String }

-- | options for 'warp goto'
newtype GotoOptions = GotoOptions { gotoname :: String }

-- | data type for command 
data Command = Display DisplayOptions |
               Add AddOptions |
               Remove RemoveOptions |
               Goto GotoOptions

-- an abstract entity representing a point to which we can warp to
data WarpPoint = WarpPoint { _name          :: String,
                             _absFolderPath :: String } deriving (Default, Generic)

-- the main data that is loaded from JSON 
newtype WarpData = WarpData { _warpPoints :: [WarpPoint] } deriving (Default, Generic)

makeLenses ''WarpData

filePathToString :: FilePath -> String
filePathToString = encodeString

warpProgDesc :: String
warpProgDesc = "use warp to quickly setup warp points and move between them"

warpHeader :: String
warpHeader = "Warp: move around your filesystem"

exec :: IO ()
exec = execParser opts >>= run 
    where versionInfo = infoOption ("teleport version: " ++ showVersion version) (short 'v' <> long "version" <> help "Show version")
          opts        = info (helper <*> parseCommand)
                            (fullDesc  <>
                             progDesc warpProgDesc <>
                             header warpHeader)

instance FromJSON WarpPoint where

instance ToJSON WarpPoint where

instance FromJSON WarpData where

instance ToJSON WarpData where

dieJSONParseError :: FilePath -> String -> IO WarpData
dieJSONParseError path err = Turtle.die . T.pack $ "parse error in: " 
    <> show path 
    <> "\nerror:------\n" <> err

decodeWarpData :: FilePath -> IO WarpData
decodeWarpData path = (fmap eitherDecode' . BSL.readFile . filePathToString) path >>= \x ->
    case x of
        Left err -> dieJSONParseError path err
        Right json -> pure json

createWarpDataFile :: FilePath -> IO ()
createWarpDataFile jsonFilePath = saveWarpData jsonFilePath def 

loadWarpData :: FilePath -> IO WarpData
loadWarpData jsonFilePath = Turtle.testfile jsonFilePath >>= \exists ->
    if exists then
        decodeWarpData jsonFilePath
    else do
       createWarpDataFile jsonFilePath
       pure def

saveWarpData :: FilePath -> WarpData -> IO ()
saveWarpData jsonFilePath warpData = do
    Turtle.touch jsonFilePath
    let dataBytestring = A.encode warpData in
        BSL.writeFile (filePathToString jsonFilePath) dataBytestring

getWarpDataPath :: IO FilePath
getWarpDataPath = Turtle.home >>= \homeFolder -> 
    pure (homeFolder </> ".warpdata")

readFolderPath :: String -> ReadM FilePath
readFolderPath = f . fromText . T.pack
    where f = \path -> if valid path then pure path
             else readerError ("invalid path: " <> show path)

warpnameParser :: Parser String
warpnameParser = argument str
    (metavar "NAME"
    <> help "name of the warp point")

parseAddCommand :: Parser Command
parseAddCommand = Add .* AddOptions <$> folderParser <*> warpnameParser

folderParser :: Parser (Maybe String)
folderParser = optional $ strOption
    (long "path"
    <> short 'p'
    <> metavar "FOLDER"
    <> help "path to the folder to warp to")

parseDisplayCommand :: Parser Command
parseDisplayCommand = pure (Display DisplayOptions)

parseRemoveCommand :: Parser Command
parseRemoveCommand = Remove <$> (RemoveOptions <$> warpnameParser)

parseGotoCommand :: Parser Command
parseGotoCommand = Goto <$> (GotoOptions <$> warpnameParser)

parseCommand :: Parser Command
parseCommand = hsubparser 
    (command "add" (info parseAddCommand (progDesc "add a warp point")) <>
    (command "list"
        (info parseDisplayCommand (progDesc "list all warp points"))) <>
    (command "del"
        (info parseRemoveCommand (progDesc "delete a warp point"))) <>
    (command "to"
        (info parseGotoCommand (progDesc "go to a created warp point"))))

setErrorColor :: IO ()
setErrorColor = ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]    

warpPointPrint :: WarpPoint -> IO ()
warpPointPrint warpPoint = do
    -- using CLICOLOR improves accessibility for people using screen readers
    useColor <- fromMaybe "1" <$> lookupEnv "CLICOLOR"
    
    if useColor /= "0" then
        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White] 
        else pure ()
    putStr (_name warpPoint)
    
    if useColor /= "0" then
        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]    
        else pure ()
    putStr $ "\t" <> _absFolderPath warpPoint <> "\n"

folderNotFoundError :: FilePath -> IO ()
folderNotFoundError path = setErrorColor >> 
    (Turtle.die . T.pack $ ("unable to find folder: " ++ show path))

needFolderNotFileError :: FilePath -> IO ()
needFolderNotFileError path = setErrorColor >>
    (Turtle.die . T.pack $ "expected folder, not file: " ++ show path)

dieIfFolderNotFound :: FilePath -> IO ()
dieIfFolderNotFound path = do
    folderExists <- Turtle.testdir path
    fileExists <- Turtle.testfile path
    when fileExists (needFolderNotFileError path)
    unless folderExists (folderNotFoundError path)

dieWarpPointExists :: WarpPoint -> IO ()
dieWarpPointExists warpPoint  =  do
    setErrorColor
    putStrLn $ "warp point " <> _name warpPoint <> " already exists:\n"
    warpPointPrint warpPoint

runAdd :: AddOptions -> IO ()
runAdd AddOptions{..} = do
    dieIfFolderNotFound . P.decode . encodeUtf8 . T.pack $ (fromMaybe "./" folderPath)
    print "folder exists, loading warp data..."
    
    warpDataPath <- getWarpDataPath
    warpData <- loadWarpData warpDataPath
    _absFolderPath <- Turtle.realpath . P.decode . encodeUtf8 . T.pack $ (fromMaybe "./" folderPath)
    
    let existingWarpPoint = find (\warp -> _name warp == addname) (_warpPoints warpData)
    case existingWarpPoint of
        Just warpPoint -> dieWarpPointExists warpPoint
        Nothing -> do
                        putStrLn "creating warp point: \n"
                        let newWarpPoint = WarpPoint { _name = addname,
                                                       _absFolderPath = filePathToString _absFolderPath }
                        warpPointPrint newWarpPoint
                        let newWarpData = over warpPoints (newWarpPoint:) warpData
                        saveWarpData warpDataPath newWarpData
    
runDisplay :: DisplayOptions -> IO ()
runDisplay DisplayOptions = do
    warpDataPath <- getWarpDataPath
    warpData <- loadWarpData warpDataPath
    forM_ (_warpPoints warpData) warpPointPrint
    
dieWarpPointNotFound :: String ->IO ()
dieWarpPointNotFound w = setErrorColor >> (Turtle.die . T.pack)
    (w <> " warp point not found")

runRemove :: RemoveOptions -> IO ()
runRemove RemoveOptions{..} = do
    warpDataPath <- getWarpDataPath
    warpData <- loadWarpData warpDataPath
    let wantedWarpPoint = find ((/= removename) . _name) (_warpPoints warpData)
    case wantedWarpPoint of
        Nothing -> dieWarpPointNotFound removename
        Just _ ->  saveWarpData warpDataPath newWarpData where
                        newWarpData = over warpPoints (filter ((/= removename) . _name)) warpData

runGoto :: GotoOptions -> IO ()
runGoto GotoOptions{..} = do
    warpDataPath <- getWarpDataPath
    warpData <- loadWarpData warpDataPath
    let wantedWarpPoint = find (\warp -> _name warp == gotoname) (_warpPoints warpData)
    case wantedWarpPoint of
        Nothing -> dieWarpPointNotFound gotoname
        Just warpPoint -> do
                             Turtle.echo (Turtle.unsafeTextToLine . T.pack $ (_absFolderPath warpPoint))
                             Turtle.cd . Turtle.fromString $ (_absFolderPath warpPoint)
                             setWorkingDirectory . Turtle.fromString $ (_absFolderPath warpPoint)
                             Turtle.exit (Turtle.ExitFailure 2)
      
run :: Command -> IO ()
run command = 
    case command of
        Add addOpt -> runAdd addOpt
        Display listOpt -> runDisplay listOpt
        Remove removeOpt -> runRemove removeOpt
        Goto gotoOpt -> runGoto gotoOpt
