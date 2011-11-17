import Prelude hiding (sequence)
import Control.Monad (when)
import Data.Enumerator (run, ($$), sequence)
import Data.Enumerator.Binary (enumHandle)
import Data.Attoparsec.Enumerator (iterParser)
import System.IO (stdin)
import System.Exit

import Lambdiff.DiffParse (diffParser)
import Lambdiff.Process (processParseFile)
import Lambdiff.UI (renderUI)

main = do
    res <- run ((enumHandle 262144 stdin) $$ (iterParser diffParser))
    diffs <- case res of
                    Left _ -> do
                        putStrLn "Invalid diff; use unified format (`git diff` or `diff -u`)"
                        exitWith $ ExitFailure 1
                    Right d -> return d
    when (length diffs == 0) $ do
        putStrLn "No renderable diffs in input"
        exitWith $ ExitFailure 1

    renderUI $ map processParseFile diffs
