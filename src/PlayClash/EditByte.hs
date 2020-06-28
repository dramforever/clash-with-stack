module PlayClash.EditByte where

import Clash.Prelude

import PlayClash.Utils ()
import PlayClash.Utils.Intel50M

import PlayClash.Debounce (debounce)

data Nibble = LowNibble | HighNibble
    deriving (Eq, Show, Generic, NFDataX, ShowX, BitPack)

data EditorState
    = EditorState
    { esNibble  :: Nibble
    , esData    :: BitVector 8
    }
    deriving (Eq, Show, Generic, NFDataX, ShowX, BitPack)

{-# ANN topEntity Synthesize
    { t_name = "edit_byte"
    , t_inputs = [ "clk", "rst", "row" ]
    , t_output = [ "out", "sel", "seg", "col" ]
    } #-}
topEntity
    :: Clock Intel50M
    -> Reset Intel50M
    -> Signal Intel50M (BitVector 4)
    ->  ( Signal Intel50M (BitVector 8)
        , Signal Intel50M (BitVector 2)
        , Signal Intel50M (BitVector 8)
        , Signal Intel50M (BitVector 4)
        )
topEntity clk rst = exposeClockResetEnable editor clk rst enableGen

dontReset :: KnownDomain dom => (HiddenReset dom => r) -> r
dontReset = withReset (unsafeFromHighPolarity (pure False))

editor :: _
    => Signal dom (BitVector 4)     -- ^ Button row lines
    ->  ( Signal dom (BitVector 8)
        , Signal dom (BitVector 2)
        , Signal dom (BitVector 8)
        , Signal dom (BitVector 4)
        )                           -- ^ Output, display select, display segments, button col lines
editor row = (dat, sel, seg, col)
    where
        blinkCtr = dontReset $ oscillate False (SNat @25_000_000)
        nibbleCtr = (\x -> if x then LowNibble else HighNibble) <$> dontReset (oscillate False (SNat @50_000))

        stateReg = regMaybe editorInitial nextStateMaybe
        nextStateMaybe = mux pressed (liftA2 go stateReg decoded) (pure Nothing)
            where go es = fmap (\dec -> enterNibble es dec)

        dat = esData <$> stateReg

        -- dispData = fromIntegral . popCount <$> buttonDat

        dispData = liftA2 go dat nibbleCtr
            where
                go d n = case n of
                    LowNibble -> l
                    HighNibble -> h
                    where (h, l) = split d

        (buttonDat, col) = keypad row
        pressed = isRising True $ (/= 0) <$> buttonDat
        decoded = fmap swap . decodeOnehot <$> buttonDat
            where
                swap x =
                    let (h, l) = split x :: (BitVector 2, BitVector 2)
                    in l ++# h

        sel = liftA3 go stateReg blinkCtr nibbleCtr
            where
                go es b n
                    | esNibble es == n && b = 0b00
                    | otherwise = case n of
                        LowNibble -> 0b01
                        HighNibble -> 0b10
        seg = hexDisplay <$> dispData

editorInitial :: EditorState
editorInitial = EditorState HighNibble 0

enterNibble :: EditorState -> BitVector 4 -> EditorState
enterNibble es dat = EditorState newNibble newDat
    where
        newNibble = case esNibble es of
            LowNibble -> HighNibble
            HighNibble ->  LowNibble
        newDat = case esNibble es of
            LowNibble -> h ++# dat
            HighNibble ->  dat ++# l
        (h, l) = split (esData es)

decodeOnehot :: BitVector 16 -> Maybe (BitVector 4)
decodeOnehot 0b0000_0000_0000_0001 = Just 0x0
decodeOnehot 0b0000_0000_0000_0010 = Just 0x1
decodeOnehot 0b0000_0000_0000_0100 = Just 0x2
decodeOnehot 0b0000_0000_0000_1000 = Just 0x3
decodeOnehot 0b0000_0000_0001_0000 = Just 0x4
decodeOnehot 0b0000_0000_0010_0000 = Just 0x5
decodeOnehot 0b0000_0000_0100_0000 = Just 0x6
decodeOnehot 0b0000_0000_1000_0000 = Just 0x7
decodeOnehot 0b0000_0001_0000_0000 = Just 0x8
decodeOnehot 0b0000_0010_0000_0000 = Just 0x9
decodeOnehot 0b0000_0100_0000_0000 = Just 0xa
decodeOnehot 0b0000_1000_0000_0000 = Just 0xb
decodeOnehot 0b0001_0000_0000_0000 = Just 0xc
decodeOnehot 0b0010_0000_0000_0000 = Just 0xd
decodeOnehot 0b0100_0000_0000_0000 = Just 0xe
decodeOnehot 0b1000_0000_0000_0000 = Just 0xf
decodeOnehot _ = Nothing

halfBound :: forall a. FiniteBits a => a
halfBound = bit (finiteBitSize (undefined :: a) - 1)

keypad :: _
    => Signal dom (BitVector 4)                 -- Row lines
    ->  ( Signal dom (BitVector 16)
        , Signal dom (BitVector 4)
        )                                       -- Output data, Column lines
keypad inp = (out', col)
    where
        ctrReg = register (0 :: Unsigned 12) (ctrReg + 1)
        (colCtr, sampCtr) = unbundle $ (\x -> split x :: (BitVector 2, BitVector 10)) <$> ctrReg

        datReg = regEn 0 (sampCtr .==. pure halfBound) (slice d19 d4 <$> liftA2 (++#) inp' datReg)
        inp' = delay 0 . delay 0 $ inp

        valid = 0 .==. (ctrReg + 1)
        out = regEn 0 valid datReg
        out' = debounce (0.5e6 :: Unsigned 20) out

        col = gen <$> colCtr
            where
                gen 0 = 0b0001
                gen 1 = 0b0010
                gen 2 = 0b0100
                gen 3 = 0b1000
                gen _ = error "unreachable"

hexDisplay :: BitVector 4 -> BitVector 8
hexDisplay digit = case digit of
    0x0 -> 0b11000000
    0x1 -> 0b11111001
    0x2 -> 0b10100100
    0x3 -> 0b10110000
    0x4 -> 0b10011001
    0x5 -> 0b10010010
    0x6 -> 0b10000010
    0x7 -> 0b11111000
    0x8 -> 0b10000000
    0x9 -> 0b10010000
    0xa -> 0b10001000
    0xb -> 0b10000011
    0xc -> 0b11000110
    0xd -> 0b10100001
    0xe -> 0b10000110
    0xf -> 0b10001110
    _ -> error "Unreachable"