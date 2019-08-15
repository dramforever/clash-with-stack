module PlayClash.Fold where

import Clash.Prelude

{-# ANN or12 Synthesize
    { t_name = "or12"
    , t_inputs = [ PortName "in" ]
    , t_output = PortName "out"
    } #-}
or12 :: Vec 12 Bool -> Bool
or12 = fold (||)
