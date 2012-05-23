module Main where

import Data.Char
import Data.Maybe
import Control.Monad.State
import System.Directory
import System.Environment
import System.FilePath
import System.IO

import Network.HTTP

import qualified Data.Map as M

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

maybeSum :: (a -> b -> b) -> (Maybe a) -> (Maybe b) -> (Maybe b)
maybeSum f ma = maybe Nothing (\b -> Just $ maybe b (\a -> f a b) ma)

convertFromAddress :: XsdAddress -> StateT (M.Map String Environment) IO (Maybe Environment)
convertFromAddress addr =
    case addr of
      XsdFile fname ->
        if ext == ".xsd"
          then do
            content <- liftIO $ readFile fname
            convertFromContent content
          else return Nothing
        where
      XsdHttp http ->
        do
          rqst <- liftIO $ simpleHTTP $ getRequest http
          content <- liftIO $ getResponseBody rqst
          convertFromContent content
  where
    name' =
      case addr of
        XsdFile a -> a
        XsdHttp a -> a

    (dname, fname') = splitFileName name'
    (bname, ext) = splitExtension fname'
    mname = drop 1 . snd . break (== '.') . fpml $ bname

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
    
    convertFromContent :: String -> StateT (M.Map String Environment) IO (Maybe Environment)
    convertFromContent content =
      do
        mp <- get
        case M.lookup mname mp of

          Just env -> return $ Just env
          Nothing -> do
            liftIO $ print $ "Start " ++ mname 
            let dmname = "src" </> "Data" </> (map (\a -> if a == '.' then '/' else a) mname) <.> "hs"
            liftIO $ createDirectoryIfMissing True $ takeDirectory dmname
            o <- liftIO $ openFile dmname WriteMode
            let d@Document{} = resolveAllNames qualify
                             . either (error . ("not XML:\n"++)) id
                             . xmlParse' name'
                             $ content
            case runParser schema [docContent (posInNewCxt name' Nothing) d] of
              (Left msg, _) -> 
                do
                  liftIO $ hPutStrLn stderr $ name' ++ ": " ++ msg
                  return Nothing
              (Right v, []) ->
                do
                  envs <- mapM convertFromAddress $ mapMaybe toAddress $ XSD.schema_items v
                  let env = foldl combineEnv emptyEnv $ catMaybes envs
                      nenv = mkEnvironment mname v env
                      decls = convert nenv v
                      haskl = Haskell.mkModule mname v decls
                      doc   = ppModule fpmlNameConverter haskl
                  liftIO $ do
                    hPutStrLn o $ render doc
                    hClose o
                  liftIO $ print $ "Stop " ++ mname 
                  modify $ M.insert mname nenv
                  return $ Just nenv 
                where
              (Right _, _)  ->
                do
                  liftIO $ hPutStrLn stderr $ name' ++ ": parsing incomplete!"
                  return Nothing

main :: IO ()
main =
  do
    names <- getArgs
    evalStateT (mapM_ convertFromAddress $ map XsdFile names) M.empty
 
