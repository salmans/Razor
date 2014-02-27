{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Data.List (intercalate)

import Happstack.Lite
  (ServerPart,Response,serve,msum,dir,ok,toResponse,lookText,seeOther,
   setResponseCode)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Text.Parsec (parse)

import Chase (chase)
import qualified Datatypes
import WebParse (pGraphLoc,pMAtom,encodeGraphLoc)

main :: IO ()
main = serve Nothing awi

awi :: ServerPart Response
awi = msum [dir "find" $ find,dir "add" $ add,dir "error" $ error_,home]

find :: ServerPart Response
find = do
  locEncoded <- lookText "q"
  case parse pGraphLoc "" locEncoded of
    Left _ -> seeOther ("/error" :: String) $ toResponse ("" :: String)
    Right loc -> ok $ template $ do
      case chase loc of
        Just (Datatypes.Model domain facts) -> do
          H.h2 "Model found."
          H.h3 "Domain:"
          H.p $ H.toHtml $ "{" ++ intercalate ", " (map show domain) ++ "}"
          H.h3 "Facts:"
          H.ul $ foldl1 (>>) $ map (H.li . H.toHtml . show) facts
        Nothing -> H.h3 "No model found."
      H.h2 "Navigation"
      H.ul $ do
        case Datatypes.previousLoc loc of
          Just prevLoc ->
            H.li $ H.a ! A.href (H.toValue $ locURL prevLoc) $ "Previous"
          Nothing -> return undefined
        H.li $ H.a ! A.href (H.toValue $ locURL $ Datatypes.nextLoc loc) $ "Next"
      H.form ! A.action "/add" $ do
        H.input ! A.type_ "hidden" ! A.name "loc" ! A.value (H.toValue $ encodeGraphLoc loc)
        H.label $ do
          "Add constraint:"
          H.input ! A.type_ "text" ! A.name "constraint"
        H.input ! A.type_ "submit" ! A.value "Add"

add :: ServerPart Response
add = do
  locEncoded <- lookText "loc"
  constraintEncoded <- lookText "constraint"
  let result = do
        loc <- parse pGraphLoc "" locEncoded
        constraint <- parse pMAtom "" constraintEncoded
        return $ Datatypes.add loc constraint
  case result of
    Left _ -> seeOther ("/error" :: String) $ toResponse ("" :: String)
    Right newLoc -> seeOther (locURL newLoc) $ toResponse ("" :: String)

error_ :: ServerPart Response
error_ = do
  setResponseCode 400
  return $ template $ do
    H.h2 "Syntax Error"
    H.p "An error occurred trying to parse the request."

home :: ServerPart Response
home = ok $ template $ do
  H.form ! A.action "/find" $ do
    H.label $ do
      "Enter theory:"
      H.input ! A.type_ "text" ! A.name "q"
    H.input ! A.type_ "submit" ! A.value "Find"

template :: H.Html -> Response
template body = toResponse $ H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset "UTF-8"
    H.title "Atlas Web Interface"
  H.body $ do
    H.h1 "Atlas Web Interface"
    body
    H.hr
    H.footer $ H.a ! A.href "/" $ "Home"

locURL :: Datatypes.GraphLoc -> String
locURL loc = "/find?q=" ++ encodeGraphLoc loc
