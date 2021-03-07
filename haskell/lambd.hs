{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ
import Text.Regex.TDFA

let tokenRegex = [r|[@.()]|[^\s@.()]+|]
let varRegex = [r|^[^\s@.()]+$|]
let numRegex = [r|TODO|]
