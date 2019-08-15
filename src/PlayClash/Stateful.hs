module PlayClash.Stateful where

import Clash.Prelude

import PlayClash.Ports

acc :: _
    => Signal dom a
    -> Signal dom a
acc a = r
    where r = register 0 (r + a)

{-# ANN acc32 Synthesize
    { t_name = "acc"
    , t_inputs = ports [ "clk", "res", "en", "in" ]
    , t_output = port "out"
    } #-}
acc32
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Unsigned 32)
    -> Signal System (Unsigned 32)
acc32 = exposeClockResetEnable acc

fir :: _
    => Vec (n + 1) a
    -> Signal dom a
    -> Signal dom a
fir ker a = dot ker <$> bundle (window a)
    where dot m n = sum (liftA2 (*) m n)

{-# ANN fir3 Synthesize
    { t_name = "fir3"
    , t_inputs = ports [ "clk", "res", "en", "in" ]
    , t_output = port "out"
    } #-}
fir3
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Unsigned 32)
    -> Signal System (Unsigned 32)
fir3 = exposeClockResetEnable (fir (3 :> 1 :> 6 :> Nil))

inix :: _
    => Signal dom a
    -> Signal dom a
inix a = r
    where r = register 31 (r + a)

{-# ANN inix32 Synthesize
    { t_name = "inix"
    , t_inputs = ports [ "clk", "res", "en", "in" ]
    , t_output = port "out"
    } #-}
inix32
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Unsigned 32)
    -> Signal System (Unsigned 32)
inix32 = exposeClockResetEnable inix
