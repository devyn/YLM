import YLM.Data
import YLM.Data.Util (lcons)
import YLM.Interfaces.Raw
import YLM.Standard
import YLM.Repl
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (find)
import System.Environment (getEnvironment)
import System.FilePath (splitSearchPath)

fm n i s = (concat [show (Map.size s), " Standard :", show n], StateDisplay $ fm (n+1))

main = do env <- getEnvironment
          let paths = maybe ["."] (splitSearchPath . snd) $ find ((== "YST_LOAD_PATH").fst) env
          repl Raw (r paths) (StateDisplay $ fm 1)
  where r paths = Map.insert "load-path" (Right $ lcons $ map Label paths) standard
