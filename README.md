# [mortgage-utils][]

## Synopsis

Tiny Haskell library and application for calculating things regarding fixed-rate
mortgages:

* The monthly payment
* The total payment
* The total interest

Please see the Haddock docs for the library documentation.

### Application Usage

```bash
$ stack run mortgage-utils -- --help
Usage: mortgage-utils (-m|--mortgage-principal ARG)
                      (-y|--mortgage-term-in-years ARG)
                      (-r|--annual-interest-rate ARG)
  Calculatin' things for a fixed-rate mortgage, yo!

Available options:
  -h,--help                Show this help text
```

```bash
$ stack run mortgage-utils -- -m 200000 -y 30 -r 3
monthlyPayment: $ 843.21
totalPayment: $ 303,554.90
totalInterest: $ 103,554.90
```

[mortgage-utils]: https://github.com/jship/mortgage-utils
