{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           Control.Monad
import qualified Data.ByteString as BStr (ByteString) 
import qualified Data.ByteString.Conversion as Conv (toByteString')
import qualified Data.ByteString.Base64 as Base64 (decode)
import qualified Data.ByteString.Internal as BSInt (toForeignPtr)
import qualified Data.Char as Chr (chr)
import qualified Data.Vector.Storable as SV (Vector, head, unsafeFromForeignPtr)
import qualified Language.Haskell.TH as TH (ExpQ, loc_filename, location) 
import qualified Language.Haskell.TH.Quote as QQ (QuasiQuoter, QuasiQuoter (..), quoteExp, quoteDec, quotePat, quoteType)
import qualified Text.ParserCombinators.Parsec as Parsec (Parser, oneOf, parse, many, spaces, many1, eof, char, string, choice, (<|>))
import qualified Foreign.Storable as St (Storable, sizeOf) 
import qualified Foreign.ForeignPtr as FPtr (castForeignPtr)
import qualified Data.Text as Text (pack)
import qualified Text.URI as URI (renderBs, mkURI)
import qualified Text.URI.QQ as URIQQ (uri)


sizeOfElem :: (St.Storable a) => SV.Vector a -> Int
sizeOfElem vec = St.sizeOf (undefined `asTypeOf` SV.head vec)

byteStringToVector :: (St.Storable a) => BStr.ByteString -> SV.Vector a
byteStringToVector bs = vec where
    vec = SV.unsafeFromForeignPtr (FPtr.castForeignPtr fptr) (scale off) (scale len)
    (fptr, off, len) = BSInt.toForeignPtr bs
    scale = (`div` sizeOfElem vec)

mkSlice :: St.Storable a => Int -> Int -> SV.Vector a -> SV.Vector a
mkSlice = undefined

base64Alph = map Chr.chr $ [65..90] ++ [97..122] ++ [48..57] ++ [43,47]

checkBase64Chr :: Parsec.Parser Char
checkBase64Chr = Parsec.oneOf base64Alph

--base64Table = [fst symWithCodes | symWithCodes <- zip [0 .. length base64Alph] base64Alph]

checkBase64Str :: Parsec.Parser String
checkBase64Str = liftM concat $ Parsec.many1 $ Parsec.spaces >> Parsec.many1 checkBase64Chr

base64Decode :: St.Storable a => String -> SV.Vector a
base64Decode str = case Base64.decode $ Conv.toByteString' str of
  Left _ -> error "Please enter base64 encoded value"
  Right base64str -> byteStringToVector base64str

base64Exp :: String -> TH.ExpQ 
base64Exp str = do
  filename <- TH.loc_filename `fmap` TH.location
  case liftM (++ (if length (filter (== '=') str) < 3 then replicate (length (filter (== '=') str)) '=' else "")) (Parsec.parse checkBase64Str filename str) of 
    Left _ -> error "Please enter base64 encoded value"
    Right properStr -> [|base64Decode properStr|]

base64 :: QQ.QuasiQuoter 
base64 = QQ.QuasiQuoter {
  QQ.quoteExp = base64Exp ,
  QQ.quoteDec = undefined ,
  QQ.quotePat = undefined ,
  QQ.quoteType = undefined 
}

uriExp :: String -> TH.ExpQ
uriExp str = case URI.mkURI (Text.pack str) of
  Nothing -> error "Please enter well-typed URI"
  Just uri -> [| byteStringToVector $ URI.renderBs uri |]

uri :: QQ.QuasiQuoter 
uri = QQ.QuasiQuoter {
  QQ.quoteExp = uriExp ,
  QQ.quoteDec = undefined ,
  QQ.quotePat = undefined ,
  QQ.quoteType = undefined
} 
