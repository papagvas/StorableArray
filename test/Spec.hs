import           Control.Monad
import           Hedgehog
import qualified Foreign.Storable as St 
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Vector.Storable as SV (fromList)
import qualified Data.Word as Word (Word8, Word16, Word32)

import Lib

genWord8List :: Gen [Word.Word8]
genWord8List =
  let listLength = Range.linear 0 10000
  in  Gen.list listLength Gen.enumBounded

genWord16List :: Gen [Word.Word16]
genWord16List =
  let listLength = Range.linear 0 10000
  in  Gen.list listLength Gen.enumBounded

genWord32List :: Gen [Word.Word32]
genWord32List =
  let listLength = Range.linear 0 10000
  in  Gen.list listLength Gen.enumBounded

prop_eqWord8 :: Property
prop_eqWord8 = property $
  forAll genWord8List >>= \xs ->
 mkSlice 0 (length xs - 1) (SV.fromList xs) === SV.fromList xs

prop_eqWord16 :: Property
prop_eqWord16 = property $
  forAll genWord16List >>= \xs -> 
 mkSlice 0 (length xs - 1) (SV.fromList xs) === SV.fromList xs

prop_eqWord32 :: Property
prop_eqWord32 = property $
  forAll genWord32List >>= \xs ->
 mkSlice 0 (length xs - 1) (SV.fromList xs) === SV.fromList xs

main :: IO ()
main = mapM_ check [prop_eqWord8, prop_eqWord16, prop_eqWord32] >> return ()

