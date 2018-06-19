
import Test.Hspec
import Cipher

main :: IO ()
main = hspec $ do
  describe "encode" $ do
    it "encode (string key) == encoded string" $do
      encode' "Hello World" "pqr" `shouldBe` "Wucae Leiat"
  describe "decode" $ do
    it "decode (string key) == decoded string" $do
      decode' "Wucae Leiat" "pqr" `shouldBe` "Hello World"
  describe "encode/decode" $ do
    it "string == decode (encode (string key) key)" $ do
      "Hello World" `shouldBe` decode' (encode' "Hello World" "xyz") "xyz"
