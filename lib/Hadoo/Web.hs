{-# LANGUAGE OverloadedStrings #-}

module Hadoo.Web where

import Web.Scotty hiding(header, body)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import System.Directory (listDirectory, removeFile)
import qualified Data.Text.Lazy as LT
import Data.List (intersperse, sort)
import qualified Hadoo.Index as IndexView
import qualified Hadoo.Edit as EditView
import qualified Hadoo.StateLanes as StateLanes


main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev
  get "/styles.css" styles
  get "/" indexAction
  get "/demo" demoPageAction
  get "/new" newItemAction
  get "/items/:state/:nr/edit" $ do editAction
  post "/items/:state/:nr/" $ do editPostAction
  post "/items/:state/:nr/delete" $ do deleteAction
  post "/items" $ do postNewItemAction
  post "/items/:state/:nr/move/:nextState" $ do moveAction

styles :: ActionM ()
styles = do
    setHeader "Content-Type" "text/css"
    file "static/styles.css"

-- | Diese Funktion entfernt `\r` Control Characters aus den übertragenen Daten.
-- Sie müssen diese Funktion verwenden um Multiline Textinput ("content") aus einer 
-- Textarea auszulesen.
multiLineTextParam :: String -> ActionM String
multiLineTextParam paramName = fmap (filter (/='\r')) (param (LT.pack paramName))

newItemAction :: ActionM ()
newItemAction = htmlString $ EditView.createNewItemPage "Edit"

demoPageAction :: ActionM ()
demoPageAction = do
    demoPage <- liftIO (readFile "static/lanes_example.html")
    htmlString demoPage

editAction :: ActionM()
editAction = do
  state <- param "state"
  nr <- param "nr"
  currentContent <- liftIO (readFile ("data/"++ state ++"/"++ intToFileName nr ++".txt"))
  htmlString $ EditView.createEditPage currentContent (intToFileName nr) state

postNewItemAction :: ActionM()
postNewItemAction = do
  state <- param "state"
  content <- multiLineTextParam "content"
  filepaths <- liftIO (listDirectory ("data/" ++ state))
  liftIO (writeFile ("data/"++ state ++"/"++  intToFileName (fileNameToInt (head filepaths)+1) ++".txt") content)
  redirect "/"

moveAction :: ActionM()
moveAction = do
  oldstate <- param "state"
  nr <- param "nr"
  newstate <- param "nextState"
  content <- liftIO (readFile ("data/"++ oldstate ++"/"++ intToFileName nr ++".txt"))
  filepaths <- liftIO (listDirectory ("data/" ++ newstate))
  liftIO (writeFile ("data/"++ newstate ++"/"++  intToFileName (fileNameToInt (head filepaths)+1) ++".txt") content)
  liftIO (removeFile  ("data/"++ oldstate ++"/"++ intToFileName nr ++".txt"))
  redirect "/"
  
editPostAction :: ActionM()
editPostAction = do
  state <- param "state"
  nr <- param "nr"
  content <- param "content"
  liftIO (writeFile ("data/"++ state ++"/"++ nr ++".txt") content)
  redirect "/"

deleteAction :: ActionM()
deleteAction = do
  state <- param "state"
  nr <- param "nr"
  liftIO (removeFile ("data/"++ state ++"/"++  intToFileName (read nr) ++".txt"))
  htmlString $ EditView.e "h1" "deleted"
  redirect "/"


indexAction :: ActionM ()
indexAction = do

  filesDone <- liftIO (listDirectory ("data/Done"))
  filesStarted <- liftIO (listDirectory ("data/Started"))
  filesTodo <- liftIO (listDirectory ("data/Todo"))

  listTodo <- liftIO(pathListToTupelList filesTodo "data/Todo/")
  listStarted <- liftIO(pathListToTupelList filesStarted "data/Started/")
  listDone <- liftIO(pathListToTupelList filesDone "data/Done/")

  htmlString $ IndexView.createIndexPage [(StateLanes.Todo, sort listTodo),(StateLanes.Started , sort listStarted),(StateLanes.Done,sort listDone)]

-- Liste zu datenTuple
pathListToTupelList :: [FilePath] -> String-> IO [(Int, String)]
pathListToTupelList [] _ = return []
pathListToTupelList (p:ps) basicPath = do
  content <- pathToTuple p basicPath
  rest <- pathListToTupelList ps basicPath
  return (content : rest)

-- Pfad zu DatenTuple
pathToTuple :: FilePath -> String -> IO (Int, String)
pathToTuple path basicPath= do
  content <- readFile (basicPath ++ path)
  return (fileNameToInt path, content)

fileNameToInt :: FilePath -> Int
fileNameToInt path = read (take 3 path) :: Int

intToFileName :: Int -> String
intToFileName i
  | i <10 = "00"++show i
  | i <100 = "0"++show i
  | otherwise = show i

htmlString :: String -> ActionM ()
htmlString = html . LT.pack
