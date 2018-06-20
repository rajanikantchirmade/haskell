
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property
import Cipher

stringGen :: Gen String
stringGen = listOf $ elements ['a'..'z']

tupleGen :: Gen (String, String)
tupleGen = do
  a <- stringGen
  b <- stringGen
  return (a, b)

prop_encode_decode :: Property
prop_encode_decode =
  forAll tupleGen
  (\(s, k) -> (s ==  decode' (encode' s k) k))

main :: IO ()
main =  do
  hspec $ do
    describe "encode" $ do
      it "encode (string key) == encoded string" $do
        encode' "Hello World" "pqr" `shouldBe` "Wucae Leiat"
    describe "decode" $ do
      it "decode (string key) == decoded string" $do
        decode' "Wucae Leiat" "pqr" `shouldBe` "Hello World"
    describe "encode/decode" $ do
      it "string == decode (encode (string key) key)" $ do
        "Hello World" `shouldBe` decode' (encode' "Hello World" "xyz") "xyz"
  verboseCheck prop_encode_decode 
