module Hadoo.Index where

import Data.List ( intersperse )
import qualified Hadoo.StateLanes as StateLanes

-- | Type Alias für Html Strings
type Html = String

-- | Erzeugt ein Element ohne Attribute
e :: String -> Html -> Html
e tag kids = ea tag [] kids

-- |Erzeugt ein Element mit Attributen
ea :: String -> [(String, String)] -> Html -> Html
ea tag attrs kids = concat $ ["<", tag] ++ attrsHtml attrs ++ [">", kids, "</", tag, ">"]
  where attrsHtml [] = []
        attrsHtml as = " " : intersperse " " (map attrHtml as)
        attrHtml (key, value) = key ++ "='" ++ value ++ "'"



--index
createIndexPage :: [(StateLanes.State,[(Int, String)])]  -> Html
createIndexPage tuplelist  =
  "<!DOCTYPE html>" ++
  ea "html" [("lang", "en")]
    (header ++ body tuplelist )

header :: Html
header =
  e "head" $
    ea "link" [("rel", "stylesheet"), ("href", "/styles.css")] ""

body :: [(StateLanes.State,[(Int, String)])] -> Html
body tuplelist =
  e "body" $
    e "h1" "Hodoo" ++ ea "a" [("href", "/new")] "New Task" ++ container tuplelist

container :: [(StateLanes.State,[(Int, String)])] ->  Html
container tuplelist =
  ea "div" [("class", "container")] $
    takefistlane tuplelist

takefistlane :: [(StateLanes.State,[(Int, String)])] -> Html
takefistlane ((laneName, laneContent):as) =  (ea "div" [("class", "lane")] $ laneTitle ((show laneName)++ " [" ++ show (length laneContent)++ "] " ) ++ itemsfirst laneName laneContent) ++ lanes as

lanes :: [(StateLanes.State,[(Int, String)])] -> Html
lanes [] = "<br>"
lanes [(laneName, laneContent)] = ea "div" [("class", "lane lastlane")] $ laneTitle ((show laneName)++ " [" ++ show (length laneContent)++ "] " ) ++ itemslast laneName laneContent
lanes ((laneName, laneContent):as) = (ea "div" [("class", "lane")] $ laneTitle ((show laneName)++ " [" ++ show (length laneContent)++ "] " ) ++ items laneName laneContent) ++ lanes as

laneTitle :: String -> Html
laneTitle title = ea "div" [("class", "title")] title

items :: StateLanes.State -> [(Int, String)]  -> Html
items _ [] = "<br>"
items laneName ((index,content):as) = ea "div" [("class", "item")] (itemcontent index laneName content) ++ items laneName as

itemslast :: StateLanes.State -> [(Int, String)]  -> Html
itemslast _ [] = "<br>"
itemslast laneName ((index,content):as) = ea "div" [("class", "item")] (lastitemcontent index laneName content) ++ itemslast laneName as

itemsfirst :: StateLanes.State -> [(Int, String)]  -> Html
itemsfirst _ [] = "<br>"
itemsfirst laneName ((index,content):as) = ea "div" [("class", "item")] (firstitemcontent index laneName content) ++ itemsfirst laneName as


itemcontent :: Int -> StateLanes.State -> String -> Html
itemcontent index laneName content =e "pre" (content) ++
    button ("/items/"++ show laneName ++"/"++ show index ++"/move/"++ show (pred laneName)) "<" "post"++
    button ("/items/"++ show laneName ++"/"++ show index ++"/move/"++ show (succ laneName)) ">" "post"++
    button ("/items/"++ show laneName ++"/"++ show index ++"/edit") "Edit" "get"++
    button ("/items/"++ show laneName ++"/"++ show index ++ "/delete") "Delete" "post"

firstitemcontent :: Int -> StateLanes.State -> String -> Html
firstitemcontent index laneName content =e "pre" (content) ++
    button ("/items/"++ show laneName ++"/"++ show index ++"/move/"++ show (succ laneName)) ">" "post"++
    button ("/items/"++ show laneName ++"/"++ show index ++"/edit") "Edit" "get"++
    button ("/items/"++ show laneName ++"/"++ show index ++ "/delete") "Delete" "post"

lastitemcontent :: Int -> StateLanes.State -> String -> Html
lastitemcontent index laneName content =e "pre" (content) ++
    button ("/items/"++ show laneName ++"/"++ show index ++"/move/"++ show (pred laneName)) "<" "post" ++
    button ("/items/"++ show laneName ++"/"++ show index ++"/edit") "Edit" "get"++
    button ("/items/"++ show laneName ++"/"++ show index ++ "/delete") "Delete" "post"


button :: String -> String -> String -> Html
button url content method=  ea "form"[("action", url),("method", method), ("class", "inline")] $
    ea "button" [("type", "submit")] content
