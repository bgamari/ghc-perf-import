module MRX (mrx) where

import Text.Regex.PCRE.Light
import Language.Haskell.TH.Quote
import Control.Lens.Regex


-- | A 'QuasiQuoter' from compiling multiline regular expressions.
mrx :: QuasiQuoter
mrx = mkRegexQQ [multiline]

