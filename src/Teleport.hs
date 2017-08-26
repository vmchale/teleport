{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Teleport where

import           Control.Lens              hiding (argument)
import           Control.Monad
import           Data.Binary
import qualified Data.ByteString.Lazy      as BSL
import           Data.Composition
import           Data.Default
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Text.Encoding
import           Data.Version
import           Filesystem                as P
import qualified Filesystem.Path.CurrentOS as P
import           GHC.Generics
import           Options.Applicative
import           Paths_shift
import           Prelude                   hiding (FilePath)
import           System.Console.ANSI
import           System.Environment
import           Turtle                    hiding (find, header)

-- | options for 'warp add'
data AddOptions = AddOptions { folderPath :: Maybe String,
                               addname    :: String }

-- | options for 'warp remove'
newtype RemoveOptions = RemoveOptions { removename :: String }

-- | options for 'warp goto'
newtype GotoOptions = GotoOptions { gotoname :: String }

-- | data type for command
data Command = Display | Add AddOptions | Remove RemoveOptions | Goto GotoOptions

-- an abstract entity representing a point to which we can warp to
data WarpPoint = WarpPoint { _name          :: String,
                             _absFolderPath :: String } deriving (Default, Generic, Binary)

-- the main data that is loaded from JSON
newtype WarpData = WarpData { _warpPoints :: [WarpPoint] } deriving (Default, Generic, Binary)

makeLenses ''WarpData
makeLenses ''WarpPoint

exec :: IO ()
exec = execParser opts >>= run
    where versionInfo = infoOption ("teleport version: " ++ showVersion version) (short 'v' <> long "version" <> help "Show version")
          opts        = info (helper <*> versionInfo <*> parseCommand)
                            (fullDesc
                            <>progDesc "use warp to quickly setup warp points and move between them"
                            <> header "Warp: move around your filesystem")

dieJSONParseError :: FilePath -> String -> IO WarpData
dieJSONParseError path err = die . T.pack . foldr (<>) mempty $
    ["parse error in: "
    , show path
    , "\nerror:      \n" <> err ]

decodeWarpData :: FilePath -> IO WarpData
decodeWarpData = fmap decode . BSL.readFile . P.encodeString

loadWarpData :: FilePath -> IO WarpData
loadWarpData jsonFilePath = testfile jsonFilePath >>= \exists ->
    if exists then decodeWarpData jsonFilePath
    else saveWarpData jsonFilePath def >> pure def

saveWarpData :: FilePath -> WarpData -> IO ()
saveWarpData jsonFilePath warpData = touch jsonFilePath >>
    let dataBytestring = encode warpData in
        BSL.writeFile (P.encodeString jsonFilePath) dataBytestring

warpDataPath :: IO FilePath
warpDataPath = home >>= \homeFolder ->
    pure (homeFolder </> ".warpdata")

readFolderPath :: String -> ReadM FilePath
readFolderPath = f . fromText . T.pack
    where f path = if P.valid path then pure path else readerError ("invalid path: " <> show path)

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

parseRemoveCommand :: Parser Command
parseRemoveCommand = Remove . RemoveOptions <$> warpnameParser

parseGotoCommand :: Parser Command
parseGotoCommand = Goto . GotoOptions <$> warpnameParser

parseCommand :: Parser Command
parseCommand = hsubparser
    (command "add" (info parseAddCommand (progDesc "add a warp point"))
    <> (command "list" (info (pure Display) (progDesc "list all warp points")))
    <> (command "del" (info parseRemoveCommand (progDesc "delete a warp point")))
    <> (command "to" (info parseGotoCommand (progDesc "go to a created warp point"))))

setErrorColor :: IO ()
setErrorColor = setSGR [SetColor Foreground Vivid Red]

colorWhen :: IO () -> IO ()
colorWhen act = do
    useColor <- fromMaybe "1" <$> lookupEnv "CLICOLOR"
    if useColor /= "0" then act else pure def

warpPointPrint :: WarpPoint -> IO ()
warpPointPrint warpPoint = do
    colorWhen $ setSGR [SetColor Foreground Dull White]
    putStr (_name warpPoint)
    colorWhen $ setSGR [SetColor Foreground Vivid Blue]
    putStr $ "\t" <> _absFolderPath warpPoint <> "\n"

folderNotFoundError :: FilePath -> IO ()
folderNotFoundError path = setErrorColor >>
    (die . T.pack $ ("unable to find folder: " ++ show path))

needFolderNotFileError :: FilePath -> IO ()
needFolderNotFileError path = setErrorColor >>
    (die . T.pack $ "expected folder, not file: " ++ show path)

dieIfFolderNotFound :: FilePath -> IO ()
dieIfFolderNotFound path = foldr (>>) (pure def)
    [ flip when (needFolderNotFileError path) =<< testfile path
    , flip unless (folderNotFoundError path) =<< testdir path ]

dieWarpPointExists :: WarpPoint -> IO ()
dieWarpPointExists warpPoint  = foldr (>>) (pure def)
    [ setErrorColor
    , putStrLn $ "warp point " <> _name warpPoint <> " already exists:\n"
    , warpPointPrint warpPoint ]

runAdd :: AddOptions -> IO ()
runAdd AddOptions{..} = do
    dieIfFolderNotFound . P.decode . encodeUtf8 . T.pack . fromMaybe "./" $ folderPath
    print "folder exists, loading warp data..."
    warpData <- loadWarpData =<< warpDataPath
    _absFolderPath <- realpath . P.decode . encodeUtf8 . T.pack . fromMaybe "./" $ folderPath
    let existingWarpPoint = find ((==addname) . _name) (_warpPoints warpData)
    case existingWarpPoint of
        Just warpPoint -> dieWarpPointExists warpPoint
        Nothing -> do
                        putStrLn "creating warp point: \n"
                        let newWarpPoint = def & name .~ addname & absFolderPath .~ P.encodeString _absFolderPath
                        warpPointPrint newWarpPoint
                        let newWarpData = over warpPoints (newWarpPoint:) warpData
                        flip saveWarpData newWarpData =<< warpDataPath

runDisplay :: IO ()
runDisplay = do
    warpData <- loadWarpData =<< warpDataPath
    forM_ (_warpPoints warpData) warpPointPrint

dieWarpPointNotFound :: String ->IO ()
dieWarpPointNotFound w = setErrorColor >> (die . T.pack)
    (w <> " warp point not found")

runRemove :: RemoveOptions -> IO ()
runRemove RemoveOptions{..} = do
    warpPath <- warpDataPath
    warp <- loadWarpData warpPath
    let wantedWarpPoint = find ((/= removename) . _name) (_warpPoints warp)
    case wantedWarpPoint of
        Nothing -> dieWarpPointNotFound removename
        Just _ -> saveWarpData warpPath
            (over warpPoints (filter ((/= removename) . _name)) warp)

runGoto :: GotoOptions -> IO ()
runGoto GotoOptions{..} = do
    warpPath <- warpDataPath
    warp <- loadWarpData warpPath
    let wantedWarpPoint = find ((== gotoname) . _name) (_warpPoints warp)
    case wantedWarpPoint of
        Nothing -> dieWarpPointNotFound gotoname
        Just warpPoint -> do
                             echo (unsafeTextToLine . T.pack . _absFolderPath $ warpPoint)
                             cd . fromString $ _absFolderPath warpPoint
                             setWorkingDirectory . fromString . _absFolderPath $ warpPoint
                             exit (ExitFailure 2)

run :: Command -> IO ()
run (Add addOpt)       = runAdd addOpt
run Display            = runDisplay
run (Remove removeOpt) = runRemove removeOpt
run (Goto gotoOpt)     = runGoto gotoOpt
