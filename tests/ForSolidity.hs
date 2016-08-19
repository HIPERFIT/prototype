module ForSolidity where

import Contract
import Contract.Transform
import Contract.Analysis
import Contract.Expr
import CodeGen.OpenclGen
import CodeGen.DataGen
import CodeGen.Utils

monthly = map (*30) [0..]
    
rentContract start years deposit rent =
    ( start
    , allCs [ payDeposit
            , foreach (take (12 * years) monthly) payRent
            , transl (years * 365) getDepositBack
            ]
    )
        where
          payDeposit = scale (r deposit) $ transfOne DKK "me" "landlord"
          payRent = scale (r rent) $ transfOne DKK "me" "landlord"
          apartmentIsOK = chosenBy ("landlord",0)
          getDepositBack = iff apartmentIsOK  (dual payDeposit) zero

theRent = rentContract (read "2014-11-01") 1 12400 6200

theRentCashflows = putStrLn $ ppCashflows $ cashflows theRent
