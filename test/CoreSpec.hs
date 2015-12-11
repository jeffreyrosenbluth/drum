module CoreSpec where

import Test.Hspec
import Test.QuickCheck
import Core

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "associative property" $ do
    it "checks for Chain" $
      property $
        \x y z -> ((x >> y) >> z) == ((x :: Song) >> ((y :: Song) >> (z :: Song)))

    it "fail" $ do 1 `shouldBe` (2 :: Int)
