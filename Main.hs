module Main where

import Control.Applicative

import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Util
import Text.XML.HaXml.Posn

datamodelFile = "specify_datamodel.xml"

main = do
  doc <- docContent (posInNewCxt datamodelFile Nothing) <$> xmlParse datamodelFile <$> readFile datamodelFile
  putStrLn $ show $ length $ tag "database" /> tag "table" $ doc
