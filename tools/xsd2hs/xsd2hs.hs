module Main where

import Data.Char
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Control.Monad

import Text.XML.HaXml            (version)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces (resolveAllNames,qualify
                                 ,nullNamespace)
import Text.XML.HaXml.Parse      (xmlParse')
import Text.XML.HaXml.Util       (docContent)
import Text.XML.HaXml.Posn       (posInNewCxt)

import Text.XML.HaXml.Schema.Parse
import Text.XML.HaXml.Schema.Environment
import Text.XML.HaXml.Schema.NameConversion
import Text.XML.HaXml.Schema.TypeConversion
import Text.XML.HaXml.Schema.PrettyHaskell
import qualified Text.XML.HaXml.Schema.HaskellTypeModel as Haskell
import Text.ParserCombinators.Poly
import Text.PrettyPrint.HughesPJ (render,vcat)

usage :: IO a
usage =
  do
    prog <- getProgName
    putStrLn $ "usage: " ++ prog ++ "<dir>"
    exitFailure

getDirName :: IO String
getDirName = 
  do
    args <- getArgs
    case args of
      [] -> usage
      x:xs -> return x

extractModuleName :: String -> Maybe String
extractModuleName str =
    if tl == ".xsd"
      then Just $ loop hd True ""
      else Nothing
  where
    (hd, tl) = break (== '.') str

    upper :: Bool -> Char -> Char
    upper True = toUpper
    upper False = id

    loop :: String -> Bool -> String -> String
    loop "" _ s = s
    loop (x:xs) b s =
      if (x == '-')
        then loop xs True s
        else loop xs False $ (upper b x):s

convertFile :: FilePath -> String -> IO ()
convertFile dir name =
  case extractModuleName name of
    Just mname ->
      do
        content <- readFile $ dir ++ "/" ++ name
        o <- openFile ("src/Data/Avaya/" ++ mname ++ ".hs") WriteMode
        let d@Document{} = resolveAllNames qualify
                         . either (error . ("not XML:\n"++)) id
                         . xmlParse' name
                         $ content
        case runParser schema [docContent (posInNewCxt name Nothing) d] of
          (Left msg, _) ->  hPutStrLn stderr msg
          (Right v, []) ->
            hPutStrLn o $ render doc
           where
            decls = convert (mkEnvironment mname v emptyEnv) v
            haskl = Haskell.mkModule mname v decls
            doc   = ppModule simpleNameConverter haskl
          (Right v, _)  -> hPutStrLn stderr "Parse incomplete!"
    Nothing -> return ()

main :: IO ()
main =
  do
    name <- getDirName
    filelist <- getDirectoryContents name
    mapM_ (convertFile name) filelist
    
