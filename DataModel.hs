module DataModel where

import Control.Applicative
import Data.Maybe
import Safe

import Data.List.Split (splitOn)

import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Util
import Text.XML.HaXml.Posn

data Table = Table { className :: String
                   , table :: String
                   , tableId :: Int
                   , searchable :: Bool
                   , businessRule :: Maybe String
                   , abbreviation :: String
                   , displayInfo :: Maybe DisplayInfo
                   , fields :: [Field]
                   } deriving (Eq)

data Field = Field { fieldName :: String
                   , column :: String
                   , javaType :: String
                   , fieldLength :: Maybe Int
                   , indexed :: Bool
                   , unique :: Bool
                   , required :: Bool
                   }
           | Relationship { fieldName :: String
                          , relType :: RelType
                          , required :: Bool
                          , relatedClassName :: String
                          , columnMaybe :: Maybe String
                          , otherSideName :: Maybe String
                          }
           | IdField { fieldName :: String
                     , column :: String
                     , javaType :: String
                     }
           deriving (Eq, Show)

data RelType = OneToOne
             | ManyToOne
             | OneToMany
             | ManyToMany
             | ZeroToOne
             deriving Eq

data DisplayInfo = DisplayInfo { view :: Maybe String
                               , uiFormatter :: Maybe String
                               , searchDialog :: Maybe String
                               , newObjectDialog :: Maybe String
                               } deriving (Eq, Show)

instance Show Table where
  show = tableName

instance Show RelType where
  show OneToOne   = "one-to-one"
  show ManyToOne  = "many-to-one"
  show OneToMany  = "one-to-many"
  show ManyToMany = "many-to-many"
  show ZeroToOne  = "zero-to-one"

instance Read RelType where
  readsPrec _ "one-to-one"   = [(OneToOne    , "")]
  readsPrec _ "many-to-one"  = [(ManyToOne   , "")]
  readsPrec _ "one-to-many"  = [(OneToMany   , "")]
  readsPrec _ "many-to-many" = [(ManyToMany  , "")]
  readsPrec _ "zero-to-one"  = [(ZeroToOne   , "")]

loadDataModel :: String -> IO [Table]
loadDataModel datamodelFile = do
  doc <- docContent (posInNewCxt datamodelFile Nothing)
         <$> xmlParse datamodelFile
         <$> readFile datamodelFile
  return $ map makeTable $ tag "database" /> tag "table" $ doc

makeTable :: Content i -> Table
makeTable node@(CElem (Elem (N "table") attrs _) _) =
  Table { className = fromJust $ getAttr "classname"
        , table = fromJust $ getAttr "table"
        , tableId = read $ fromJust $ getAttr "tableid"
        , searchable = "true" == (fromJust $ getAttr "searchable")
        , businessRule = getAttr "businessrule"
        , abbreviation = fromJust $ getAttr "abbrv"
        , displayInfo = fmap makeDisplayInfo (headMay $ tag "table" /> tag "display" $ node)
        , fields = (map makeField $ tag "table" /> tag "field" $ node) ++
                   (map makeField $ tag "table" /> tag "id" $ node) ++
                   (map makeField $ tag "table" /> tag "relationship" $ node)
        }
  where getAttr name = show <$> lookup (N name) attrs

getAttr name attrs = show <$> lookup (N name) attrs

makeDisplayInfo :: Content i -> DisplayInfo
makeDisplayInfo (CElem (Elem (N "display") attrs _) _) =
  DisplayInfo { view = getAttr "view" attrs
              , uiFormatter = getAttr "uiformatter" attrs
              , searchDialog = getAttr "searchdlg" attrs
              , newObjectDialog = getAttr "newobjdlg" attrs
              }

makeField :: Content i -> Field
makeField (CElem (Elem (N "id") attrs _) _) =
  IdField { fieldName = fromJust $ getAttr "name" attrs
          , column = fromJust $ getAttr "column" attrs
          , javaType = fromJust $ getAttr "type" attrs
          }

makeField (CElem (Elem (N "field") attrs _) _) =
  Field { fieldName = fromJust $ getAttr "name" attrs
        , column = fromJust $ getAttr "column" attrs
        , indexed = "true" == (fromJust $ getAttr "indexed" attrs)
        , unique = "true" == (fromJust $ getAttr "unique" attrs)
        , required = "true" == (fromJust $ getAttr "required" attrs)
        , javaType = fromJust $ getAttr "type" attrs
        , fieldLength = read <$> getAttr "length" attrs
        }

makeField (CElem (Elem (N "relationship") attrs _) _) =
  Relationship { fieldName = fromJust $ getAttr "relationshipname" attrs
               , relType = read $ fromJust $ getAttr "type" attrs
               , required = "true" == (fromJust $ getAttr "required" attrs)
               , relatedClassName = fromJust $ getAttr "classname" attrs
               , columnMaybe = getAttr "columnname" attrs
               , otherSideName = getAttr "othersidename" attrs
               }

getFieldFromTable :: Table -> String -> Field
getFieldFromTable table fldName = case filter byName $ fields table of
  [] -> error $ (tableName table) ++ " has no field named "
        ++ fldName ++ ". Existing fields: "
        ++ (show $ map fieldName $ fields table)
  [field] -> field
  where byName = (==) fldName . fieldName

getTable :: [Table] -> String -> Table
getTable tables name = head $ filter byName tables
  where byName = (==) name . tableName

tableName :: Table -> String
tableName = last . splitOn "." . className
