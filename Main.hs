import YLM.Data
import YLM.Interfaces.Raw
import YLM.Standard
import YLM.Repl
import Data.Map (Map)
import qualified Data.Map as Map

fm n i s = (concat [show (Map.size s), " Standard :", show n], StateDisplay $ fm (n+1))

main = repl Raw standard (StateDisplay $ fm 1)
