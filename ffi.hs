{-| This module is the primary user interface library
-}
module FFI where
import System.Environment
import System.Console.GetOpt
import System.Exit (exitWith, ExitCode (..))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List
import Formula.SyntaxGeo (Theory, Sequent, Term, Elem, parseSequent)
import Utils.Utils (isRealLine)
import Tools.Config
import Tools.Counter
import Tools.FolToGeo
import qualified Chase.Problem.Model as Model
import Chase.Chase (chase, chase', chaseWithModel)
import Chase.Problem.XMLModel
-- HXT Library Modules
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Core
-- FFI Modules
import Foreign.C.Types
import Foreign.C.String

getmodels_hs :: CString -> IO ()
getmodels_hs cs = do
  s <- peekCString cs
  getmodels s

getmodels :: String -> IO ()
getmodels inputFileName = do
  -- Get the geometric formulas from the input file
  geoFmlas <- geoFormulas inputFileName
  case geoFmlas of
    Just fs -> runChase defaultConfig fs []
    Nothing -> error "The input is not geometric!"
  

-- Runs the chase according to given parameters
runChase :: Config -> Theory -> [Elem] -> IO ()
runChase config fmlas elems = printModels $ chase config fmlas
                
-- Sends a list of models to IO
printModels :: [Model.Model] -> IO ()
printModels []   = putStrLn "No models found!"
printModels mdls = do
  mapM_ (\m -> do 
          putStrLn ("PRETTY")
          putStrLn (show m)
          putStrLn ("XML")
          putStrLn (showPickled [] m)
          putStrLn ("\n" ++ "--------------------")) mdls

-- Loads geometric formulas from an input file
geoFormulas :: String -> IO (Maybe Theory)
geoFormulas fName = do
  src <- readFile fName
  let inputLines = lines src
      realLines  = filter isRealLine inputLines
      inputFmlas = mapM (parseFolToSequent False) realLines
  return $ concat <$> inputFmlas

foreign export ccall getmodels_hs :: CString -> IO ()