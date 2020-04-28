module PlayClash.UpDown where

import Clash.Prelude

import PlayClash.Utils ()

{-# ANN topEntity Synthesize
    { t_name = "up_down"
    , t_inputs = ["clk", "rst", "in"]
    , t_output = "out"
    } #-}
topEntity
    :: Clock XilinxSystem
    -> Reset XilinxSystem
    -> Signal XilinxSystem Bool
    -> Signal XilinxSystem (Unsigned 2)
topEntity clk rst inp =
    exposeClockResetEnable
        (upDown 1 inp)
        clk rst enableGen

upDown :: _
    => a
    -> Signal dom Bool
    -> Signal dom a
upDown initial = mealy go initial
    where
        go x True = (x + 1, x + 1)
        go x False = (x - 1, x - 1)
