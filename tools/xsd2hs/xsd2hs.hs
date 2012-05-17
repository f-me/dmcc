module Main where

import Data.Char
import Data.Maybe
import System.Environment
import System.FilePath
import System.IO

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

convertFile :: FilePath -> IO (Maybe Environment)
convertFile fname =
    if ext == ".xsd"
      then
        do
          content <- readFile fname
          o <- openFile ("src/Data/Avaya/Generated/" ++ mname ++ ".hs") WriteMode
          let d@Document{} = resolveAllNames qualify
                           . either (error . ("not XML:\n"++)) id
                           . xmlParse' fname'
                           $ content
          case runParser schema [docContent (posInNewCxt fname' Nothing) d] of
            (Left msg, _) -> 
              do
                hPutStrLn stderr $ fname ++ ": " ++ msg
                return Nothing
            (Right v, []) ->
              do
                envs <- mapM convertFile $ mapMaybe fromInclude $ XSD.schema_items v
                let env = foldl combineEnv emptyEnv $ catMaybes envs
                    nenv = mkEnvironment mname v env
                    decls = convert nenv v
                    haskl = Haskell.mkModule ("Data.Avaya.Generated." ++ mname) v decls
                    doc   = ppModule simpleNameConverter haskl
                hPutStrLn o $ render doc
                hFlush o
                hPutStrLn stderr $ "Generated haskell file from " ++ fname
                return $ Just nenv 
             where
              fromInclude (XSD.Include a _) = Just $ dname </> a
              fromInclude _ = Nothing
            (Right _, _)  ->
              do
                hPutStrLn stderr $ fname ++ ": parsing incomplete!"
                return Nothing
      else return Nothing
  where
    (dname, fname') = splitFileName fname
    (bname, ext) = splitExtension fname'
    mname = moduleName bname

main :: IO ()
main =
  do
    names <- getArgs
    mapM_ convertFile names
    
