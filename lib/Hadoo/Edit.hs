module Hadoo.Edit where

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


--NewItem
createNewItemPage :: String -> Html
createNewItemPage title =
  "<!DOCTYPE html>" ++
  ea "html" [("lang", "en")]
    (e "h1" title ++ textForm "/items" "" "senden")

textForm :: String -> String -> String -> Html
textForm postPath textContent buttonString =
  ea "form" [("action", postPath),("method", "post"), ("class", "inline")] $
  ea "select" [("name", "state")] $ (option "Todo" ++ option "Started" ++ option "Done") ++
  ea "textarea" [("name", "content"), ("rows", "12"), ("cols", "60")] textContent ++
  ea "button" [("type", "submit")] buttonString

option :: String -> Html
option value =  ea "option" [("value", value)] value


--EditPage
createEditPage :: String -> String -> String -> Html
createEditPage currentContent nr state=
  "<!DOCTYPE html>" ++
  ea "html" [("lang", "en")]
    (e "h1" "Edit") ++ textForm ("/items/"++ state ++"/"++nr++"/") currentContent "save"