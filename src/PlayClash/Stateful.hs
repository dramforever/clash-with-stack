{-# LANGUAGE RankNTypes #-}

module PlayClash.Stateful where

import Clash.Prelude

type Sys = XilinxSystem

acc :: _
    => Signal dom (Unsigned 32) -> Signal dom (Unsigned 32)
acc a = r
    where r = register 0 (r + a)

{-# ANN acc32 Synthesize
    { t_name = "acc"
    , t_inputs =
        [ PortName "clk"
        , PortName "rst"
        , PortName "in"
        ]
    , t_output = PortName "out"
    } #-}
acc32
    :: Clock Sys
    -> Reset Sys
    -> Signal Sys (Unsigned 32)
    -> Signal Sys (Unsigned 32)
acc32 clk rst = exposeClockResetEnable (acc @Sys) clk rst enableGen

fir :: _
    => a -> a -> a
    -> Signal dom a
    -> Signal dom a
fir p1 p2 p3 a1 = pure p1 * a1 + pure p2 * a2 + pure p3 * a3
    where
        a2 = register 0 a1
        a3 = register 0 a2

{-# ANN fir3 Synthesize
    { t_name = "fir3"
    , t_inputs =
        [ PortName "clk"
        , PortName "rst"
        , PortName "in"
        ]
    , t_output = PortName "out"
    } #-}
fir3
    :: Clock Sys
    -> Reset Sys
    -> Signal Sys (Unsigned 32)
    -> Signal Sys (Unsigned 32)
fir3 clk rst = exposeClockResetEnable (fir 3 1 6) clk rst enableGen

inix
    ::
     ( HiddenClockResetEnable dom
     , Num a, NFDataX a
     )
    => Signal dom a
    -> Signal dom a
inix a = r
    where r = register 31 (r + a)

{-# ANN inix32 Synthesize
    { t_name = "inix"
    , t_inputs =
        [ PortName "clk"
        , PortName "rst"
        , PortName "in"
        ]
    , t_output = PortName "out"
    } #-}
inix32
    :: Clock Sys
    -> Reset Sys
    -> Signal Sys (Unsigned 32)
    -> Signal Sys (Unsigned 32)
inix32 clk rst = exposeClockResetEnable inix clk rst enableGen
