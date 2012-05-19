module Main where

import Data.Char
import Data.Maybe
import System.Environment
import System.FilePath
import System.IO

import Network.HTTP

import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces (resolveAllNames,qualify)
import Text.XML.HaXml.Parse      (xmlParse')
import Text.XML.HaXml.Util       (docContent)
import Text.XML.HaXml.Posn       (posInNewCxt)

import Text.XML.HaXml.Schema.Parse
import Text.XML.HaXml.Schema.Environment
import Text.XML.HaXml.Schema.NameConversion
import Text.XML.HaXml.Schema.TypeConversion
import Text.XML.HaXml.Schema.PrettyHaskell
import qualified Text.XML.HaXml.Schema.HaskellTypeModel as Haskell
import qualified Text.XML.HaXml.Schema.XSDTypeModel as XSD
import Text.ParserCombinators.Poly
import Text.PrettyPrint.HughesPJ (render)

data XsdAddress = XsdFile FilePath | XsdHttp String

xsdName :: String -> Maybe String
xsdName str =
    if tl == ".xsd"
      then Just hd
      else Nothing
  where
    (hd, tl) = break (== '.') str

moduleName :: String -> String
moduleName str =
    loop str True
  where
    upper :: Bool -> Char -> Char
    upper True = toUpper
    upper False = id

    loop "" _ = ""
    loop (x:xs) b =
      if (x == '-')
        then loop xs True
        else (upper b x):(loop xs False)

maybeSum :: (a -> b -> b) -> (Maybe a) -> (Maybe b) -> (Maybe b)
maybeSum f ma = maybe Nothing (\b -> Just $ maybe b (\a -> f a b) ma)

convertFromAddress :: XsdAddress -> IO (Maybe Environment)
convertFromAddress addr =
    case addr of
      XsdFile fname ->
        if ext == ".xsd"
          then do
            content <- readFile fname
            convertFromContent content
          else return Nothing
        where
      XsdHttp http ->
        do
          rqst <- simpleHTTP $ getRequest http
          content <- getResponseBody rqst
          convertFromContent content
  where
    name' =
      case addr of
        XsdFile a -> a
        XsdHttp a -> a

    (dname, fname') = splitFileName name'
    (bname, ext) = splitExtension fname'
    mname = moduleName bname

    toAddress (XSD.Include a _) =
      case addr of
        XsdFile _ -> Just $ XsdFile $ dname </> a
        XsdHttp _ -> Just $ XsdHttp $ dname </> a
    toAddress (XSD.Import _ a _) =
       if dname' == "." 
         then Just $ XsdHttp $ dname </> a
         else Just $ XsdHttp a
      where
        dname' = takeDirectory a
    toAddress _ = Nothing
    
    convertFromContent :: String -> IO (Maybe Environment)
    convertFromContent content =
      do
        o <- openFile ("src/Data/Avaya/Generated/" ++ mname ++ ".hs") WriteMode
        let d@Document{} = resolveAllNames qualify
                         . either (error . ("not XML:\n"++)) id
                         . xmlParse' name'
                         $ content
        case runParser schema [docContent (posInNewCxt name' Nothing) d] of
          (Left msg, _) -> 
            do
              hPutStrLn stderr $ name' ++ ": " ++ msg
              return Nothing
          (Right v, []) ->
            do
              hPutStrLn stderr $ "Generating haskell code from " ++ name'
              envs <- mapM convertFromAddress $ mapMaybe toAddress $ XSD.schema_items v
              let env = foldl combineEnv emptyEnv $ catMaybes envs
                  nenv = mkEnvironment mname v env
                  decls = convert nenv v
                  haskl = Haskell.mkModule ("Data.Avaya.Generated." ++ mname) v decls
                  doc   = ppModule simpleNameConverter haskl
              hPutStrLn o $ render doc
              hPutStrLn stderr $ "Generated haskell code from " ++ name'
              hClose o
              return $ Just nenv 
            where
          (Right _, _)  ->
            do
              hPutStrLn stderr $ name' ++ ": parsing incomplete!"
              return Nothing

main :: IO ()
main =
  do
    names <- getArgs
    mapM_ convertFromAddress $ map XsdFile names
    
