module FindSpec (spec) where

import Test.Hspec
import Find

spec :: Spec
spec = do
  describe "absolute" $ do
    it "no input and no search string retuns nothing" $
      searchFile "" "" `shouldBe` []

    it "returns nothing when string isn't in the search string" $
      searchFile "not" "cool what's up with you" `shouldBe` []

    it "search string in the only line" $
      searchFile "line" "the search string is in line 1" `shouldBe`
      [(1, "the search string is in line 1")]

    it "returns multiple lines with correct line numbers" $
      searchFile "line" "not here\nthe search string is in line 2\nnot here\nline 4" `shouldBe`
      [(2, "the search string is in line 2"), (4, "line 4")]
