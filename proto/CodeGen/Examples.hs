module CodeGen.Examples where

import Contract
import LexifiContracts
import CodeGen.OpenclGen

-- Sample contracts

ex1 = scale 200 $ flow 1 (r 100) EUR "you" "me"

ex2 =
    let strike = 4000.0
        theobs = obs ("Carlsberg",0)
    in scale (r 0.9997817434496459)
             (transl 360
                    (iff (r strike !<! theobs)
                          (scale (theobs - r strike)
                                 (transfOne EUR "you" "me"))
                         zero))

-- like ex2 but with additional condition (just to test nested "if"s)
ex3 =
    let strike = 4000.0
        theobs = obs ("Carlsberg",0)
    in scale (r 0.9997817434496459)
             (transl 360
                    (iff (r strike !<! theobs)
                          ( iff (r strike !<! theobs * 2)
                            (scale (theobs*2 - r strike) (transfOne EUR "you" "me"))
                            (scale (theobs - r strike)
                                 (transfOne EUR "you" "me")))
                         zero))

ex4 =
    let strike = 4000.0
        theobs = obs ("Carlsberg",0)
        theobs1 = obs ("Carlsberg",1)
    in scale (r 0.9997817434496459)
             (transl 360
                    (iff (r strike !<! theobs1)
                          (transl 360 $
                           iff (r strike !<! theobs * 2)
                               (scale (theobs*2 - r strike) (transfOne EUR "you" "me"))
                               (scale (theobs1 - r strike)
                                      (transfOne EUR "you" "me")))
                           (transl 180 $ (iff (r strike !<! theobs - 10)
                                              (transl 185 $ scale theobs $ transfOne EUR "you" "me")
                                          zero))))
equity = "Carlsberg"
maturity = 2
ex5 = checkWithin (strike !<! theobs) maturity
                    (scale (theobs - strike) (transfOne EUR "you" "me")) zero
    where strike = r 50.0
          theobs = obs (equity,0)

-- usage examples
-- putStr $ ppCLSeq $ genPayoffFunc ex2 -- pretty-printing in console
-- writeOpenCL (ppCLSeq $ genPayoffFunc ex2) "SmallContract" -- to generate SmallContract.cl in current directory
