module CoinChangeSpec where

import Test.Hspec
import Test.QuickCheck

import CoinChange


spec :: Spec
spec = do
   describe "when changing an amount" $ do
      it "should give change of the same value as the input-amount" $ property $
         \amount -> (sum . map pieceValue . changeFor . getNonNegative) amount == getNonNegative amount

      context "that can be given as two coins" $ do
         it "gives 2xtwo-cent for the amount 4" $ do
            changeFor 4 `shouldBe` [TwoCent, TwoCent]

instance Arbitrary Pieces where
   arbitrary = elements [ Cent, TwoCent, FiveCent
                        , TenCent, TwentyCent
                        , FiftyCent, Euro, TwoEuro]
