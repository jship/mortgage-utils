{-# LANGUAGE BlockArguments #-}

module MortgageUtilsSpec
  ( spec
  ) where

import MortgageUtils
  ( Config(Config, annualInterestRate, mortgagePrincipal, mortgageTermInYears), monthlyPayment
  , prettyUSD, totalInterest, totalPayment
  )
import Test.Hspec (shouldBe)
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do
  Hspec.describe "MortgageUtils.monthlyPayment" do
    Hspec.it "15-year, $200,000 mortgage, 3.0% annual interest: $ 1,381.16" do
      let result = monthlyPayment Config
            { mortgagePrincipal = 200000
            , mortgageTermInYears = 15
            , annualInterestRate = 3.0
            }
      prettyUSD result `shouldBe` "$ 1,381.16"
    Hspec.it "30-year, $200,000 mortgage, 3.0% annual interest: $ 843.21" do
      let result = monthlyPayment Config
            { mortgagePrincipal = 200000
            , mortgageTermInYears = 30
            , annualInterestRate = 3.0
            }
      prettyUSD result `shouldBe` "$ 843.21"
    Hspec.it "30-year, $300,000 mortgage, 4.0% annual interest: $ 1,432.25" do
      let result = monthlyPayment Config
            { mortgagePrincipal = 300000
            , mortgageTermInYears = 30
            , annualInterestRate = 4.0
            }
      prettyUSD result `shouldBe` "$ 1,432.25"
  Hspec.describe "MortgageUtils.totalPayment" do
    Hspec.it "15-year, $200,000 mortgage, 3.0% annual interest: $ 248,609.39" do
      let result = totalPayment Config
            { mortgagePrincipal = 200000
            , mortgageTermInYears = 15
            , annualInterestRate = 3.0
            }
      prettyUSD result `shouldBe` "$ 248,609.39"
    Hspec.it "30-year, $200,000 mortgage, 3.0% annual interest: $ 303,554.90" do
      let result = totalPayment Config
            { mortgagePrincipal = 200000
            , mortgageTermInYears = 30
            , annualInterestRate = 3.0
            }
      prettyUSD result `shouldBe` "$ 303,554.90"
    Hspec.it "30-year, $300,000 mortgage, 4.0% annual interest: $ 515,608.52" do
      let result = totalPayment Config
            { mortgagePrincipal = 300000
            , mortgageTermInYears = 30
            , annualInterestRate = 4.0
            }
      prettyUSD result `shouldBe` "$ 515,608.52"
  Hspec.describe "MortgageUtils.totalInterest" do
    Hspec.it "15-year, $200,000 mortgage, 3.0% annual interest: $ 48,609.39" do
      let result = totalInterest Config
            { mortgagePrincipal = 200000
            , mortgageTermInYears = 15
            , annualInterestRate = 3.0
            }
      prettyUSD result `shouldBe` "$ 48,609.39"
    Hspec.it "30-year, $200,000 mortgage, 3.0% annual interest: $ 103,554.90" do
      let result = totalInterest Config
            { mortgagePrincipal = 200000
            , mortgageTermInYears = 30
            , annualInterestRate = 3.0
            }
      prettyUSD result `shouldBe` "$ 103,554.90"
    Hspec.it "30-year, $300,000 mortgage, 4.0% annual interest: $ 215,608.52" do
      let result = totalInterest Config
            { mortgagePrincipal = 300000
            , mortgageTermInYears = 30
            , annualInterestRate = 4.0
            }
      prettyUSD result `shouldBe` "$ 215,608.52"
