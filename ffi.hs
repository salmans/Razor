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
import Foreign.Marshal.Alloc

hs_getmodels :: CString -> IO (CString)
hs_getmodels cs = do
  s <- peekCString cs
  xmlModels <- getmodels s
  cModels <- (newCString xmlModels)
  return cModels

hs_free_cstring :: CString -> IO ()
hs_free_cstring p = free p

getmodels :: String -> IO (String)
getmodels inputFileName = do
  -- Get the geometric formulas from the input file
  geoFmlas <- geoFormulas inputFileName
  case geoFmlas of
    Just fs -> return (runChase defaultConfig fs [])
    Nothing -> return ""
  
-- Runs the chase according to given parameters
runChase :: Config -> Theory -> [Elem] -> String
runChase config fmlas elems = showPickled [] (Models (chase config fmlas))
                
-- Loads geometric formulas from an input file
geoFormulas :: String -> IO (Maybe Theory)
geoFormulas fName = do
  src <- readFile fName
  let inputLines = lines src
      realLines  = filter isRealLine inputLines
      inputFmlas = mapM (parseFolToSequent False) realLines
  return $ concat <$> inputFmlas

foreign export ccall hs_getmodels :: CString -> IO (CString)
foreign export ccall hs_free_cstring :: CString -> IO ()
