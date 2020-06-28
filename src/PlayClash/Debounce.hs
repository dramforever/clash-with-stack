module PlayClash.Debounce where

import Clash.Prelude

import PlayClash.Utils ()
import PlayClash.Utils.Intel50M

-- type DebounceCounter = Unsigned 20

-- debounceMax :: DebounceCounter
-- debounceMax = 0.5e6

{-# ANN debounceBit Synthesize
    { t_name = "debounce_bit"
    , t_inputs = [ "clk", "in" ]
    , t_output = "out"
    } #-}
debounceBit
    :: Clock Intel50M
    -> Signal Intel50M Bit
    -> Signal Intel50M Bit
debounceBit clk =
    exposeClock (exposeEnable debounce)
        clk enableGen
        (0.5e6 :: Unsigned 20)

{-# ANN debounceBV4 Synthesize
    { t_name = "debounce_vec_4"
    , t_inputs = [ "clk", "in" ]
    , t_output = "out"
    } #-}
debounceBV4
    :: Clock Intel50M
    -> Signal Intel50M (BitVector 4)
    -> Signal Intel50M (BitVector 4)
debounceBV4 clk =
    exposeClock (exposeEnable debounce)
        clk enableGen
        (0.5e6 :: Unsigned 20)

debounce :: _
    => c
    -> Signal dom a
    -> Signal dom a
debounce ctrMax inpSig = snd <$> next
    where
        stateReg = delay (0, def, def) (fst <$> next)
        next = liftA2 go stateReg inpSig

        go (0, cur, out) inp
            | cur == inp = ((0, cur, cur), cur)
            | otherwise = ((ctrMax, inp, out), out)
        go (num, cur, out) inp
            | cur == inp = ((num - 1, cur, out), out)
            | otherwise = ((ctrMax, cur, out), out)
