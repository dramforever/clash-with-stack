module PlayClash.Uart where

import Clash.Prelude

import PlayClash.Utils ()
import PlayClash.Utils.Intel50M

type Byte = BitVector 8
type Counter = Unsigned 9

counterMax :: Counter
counterMax = 433

data UartState
    = Idle
    | Send Counter Byte SendState
    deriving (Eq, Show, Generic, NFDataX, ShowX)

data SendState
    = SendStart
    | SendData (Unsigned 3)
    | SendStop
    deriving (Eq, Show, Generic, NFDataX, ShowX)

{-# ANN topEntity Synthesize
    { t_name = "uarttx"
    , t_inputs = [ "clk", "rst", "din", "valid" ]
    , t_output = [ "tx", "ready" ]
    } #-}
topEntity
    :: Clock Intel50M
    -> Reset Intel50M
    -> Signal Intel50M Byte
    -> Signal Intel50M Bool
    ->  ( Signal Intel50M Bit
        , Signal Intel50M Bool
        )
topEntity clk rst inp valid =
    exposeClockResetEnable
        (uartTx inp valid)
        clk rst enableGen

uartTx :: _
    => Signal dom Byte      -- ^ The byte to send
    -> Signal dom Bool      -- ^ The /valid/ flag of the byte to send
    ->  ( Signal dom Bit
        , Signal dom Bool
        )                   -- ^ Output, /ready/ flag
uartTx inp valid = (output, ready)
    where
        output = uartOutput <$> stateReg
        ready = uartReady <$> stateReg
        stateReg = register Idle nextState
        nextState = mux (ready .&&. valid) (makeUart <$> inp) (uartStep <$> stateReg)

makeUart :: Byte -> UartState
makeUart val = Send counterMax val SendStart

uartOutput :: UartState -> Bit
uartOutput Idle = 1
uartOutput (Send _ctr val send) =
    case send of
        SendStart -> 0
        SendData i -> val ! i
        SendStop -> 1

uartStep :: UartState -> UartState
uartStep Idle = Idle
uartStep (Send 0 val send) =
    case send of
        SendStart -> more (SendData 0)
        SendData 7 -> more SendStop
        SendData i -> more (SendData $ i + 1)
        SendStop -> Idle
    where more = Send counterMax val
uartStep (Send ctr val send) = Send (ctr - 1) val send

uartReady :: UartState -> Bool
uartReady Idle = True
uartReady (Send 0 _ SendStop) = True
uartReady _ = False
