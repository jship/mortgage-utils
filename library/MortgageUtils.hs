{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MortgageUtils
  ( -- * Utilities
    -- ** Mortgage-related
    Config(Config, mortgagePrincipal, mortgageTermInYears, annualInterestRate)
  , monthlyPayment
  , totalPayment
  , totalInterest

    -- ** Miscellaneous
  , prettyUSD

    -- * Application
  , run
  , main
  ) where

import Control.Applicative ((<**>))
import Data.Currency.Pretty (Amount(Amount), USD(USD))
import Prelude
import qualified Data.Currency.Pretty as Currency
import qualified Options.Applicative as Options

data Config = Config
  { mortgagePrincipal :: Double
  , mortgageTermInYears :: Int
  , annualInterestRate :: Double -- ^ (0.0, 100.0]
  } deriving stock (Eq, Show)

-- | This is the amount paid per month until the end of the mortgage term.
monthlyPayment :: Config -> Double
monthlyPayment config = result where
  result = numerator / denominator

  numerator = mortgagePrincipal * periodicInterestRate
  denominator = 1 - (1 + periodicInterestRate) ^^ (-totalInterestPeriods)

  periodicInterestRate = annualInterestRate / 100 / 12
  totalInterestPeriods = mortgageTermInYears * 12

  Config { mortgagePrincipal, mortgageTermInYears, annualInterestRate } = config

-- | This is the total amount paid by the end of the mortgage term. Mortgages
-- have annual interest rates, so the total amount paid will be greater than
-- the mortgage principal.
totalPayment :: Config -> Double
totalPayment config = result where
  result = monthlyPayment config * fromIntegral mortgageTermInYears * 12
  Config { mortgageTermInYears } = config

-- | This is the total interest paid by the end of the mortgage term, so the
-- amount paid on top of the mortgage principal.
--
-- @totalInterest config == totalPayment config - mortgagePrincipal config@
totalInterest :: Config -> Double
totalInterest config = result where
  result = totalPayment config - mortgagePrincipal config

-- | Pretty-print a 'Double' amount as USD currency.
--
-- @prettyUSD 3.1415 == "$ 3.14"@
prettyUSD :: Double -> String
prettyUSD double =
  flip Currency.prettyPrintWith (Amount USD double) Currency.defaultConfig
    { Currency.useCurrencySymbol = True
    , Currency.compactFourDigitAmounts = False
    }

-- | Print the results of running all mortgage-related utilities against the
-- 'Config'.
run :: Config -> IO ()
run config = do
  putStrLn $ "monthlyPayment: " <> prettyUSD (monthlyPayment config)
  putStrLn $ "totalPayment: " <> prettyUSD (totalPayment config)
  putStrLn $ "totalInterest: " <> prettyUSD (totalInterest config)

-- | Parse command line arguments into a 'Config' value, then provide this
-- value to 'run'.
main :: IO ()
main = run =<< Options.execParser opts where
  opts = Options.info (configParser <**> Options.helper) . mconcat $
    [ Options.fullDesc
    , Options.progDesc
        "Calculatin' things for a fixed-rate mortgage, yo!"
    ]
  configParser = do
    mortgagePrincipal <- optionParser 'm' "mortgage-principal"
    mortgageTermInYears <- optionParser 'y' "mortgage-term-in-years"
    annualInterestRate <- optionParser 'r' "annual-interest-rate"
    pure Config
      { mortgagePrincipal
      , mortgageTermInYears
      , annualInterestRate
      }
    where
    optionParser :: Read a => Char -> String -> Options.Parser a
    optionParser shortName longName = Options.option Options.auto . mconcat $
      [ Options.short shortName
      , Options.long longName
      ]
