module PlayClash.Fold where

import Clash.Prelude
import PlayClash.Ports

{-# ANN or12 Synthesize
    { t_name = "or12"
    , t_inputs = ports [ "in" ]
    , t_output = port "out"
    } #-}
or12 :: Vec 12 Bool -> Bool
or12 = fold (||)
