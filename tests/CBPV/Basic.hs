module CBPV.Basic (basicTests, buildNat) where

import Test.HUnit

import CBPV.Helper
import Otus

mvar :: Int -> MetaTerm
mvar = MVar . IndexId

mPiTy :: [MetaTerm] -> MetaTerm -> MetaTerm
mPiTy = foldr (\dom -> (.) (MPi dom mempty)) id

mApp :: MetaTerm -> [MetaTerm] -> MetaTerm
mApp = foldl MApp

mLam :: Int -> MetaTerm -> MetaTerm
mLam 0 = id
mLam n = mLam (n - 1) . MLam

buildNat :: Int -> MetaTerm
buildNat 0 = zero
buildNat x =
  let prev = buildNat $ x - 1
  in MLetIn prev (MApp suc (mvar 0)) nat

-- defs
nat :: MetaTerm
nat =
  mPiTy
    [ MVType 0, -- A : VU 0
      mvar 0, -- z : A
      MU mempty (mPiTy [mvar 1] $ MF (mvar 2)) -- s : U(A -> F A)
    ]
    $ MF (mvar 2)

zero :: MetaTerm
zero = mLam 3 $ MReturn (mvar 1)

-- \n.\A.\z.\s. s (n A z s) -- let (n A z s) in s 0
suc :: MetaTerm
suc =
  mLam 4 $
    MLetIn
      ( mApp
          (MForce (mvar 3))
          [ mvar 2, -- A
            mvar 1, -- z
            mvar 0 -- s
          ]
      )
      (MApp (MForce $ mvar 1) (mvar 0))
      (MF $ mvar 3)

basicTests :: Test
basicTests =
  TestList
    [ TestCase $ assertCheck "nat-is-ctype" nat (MCType 10),
      TestCase $ assertCheck "zero-is-nat" zero nat,
      TestCase $ assertCheck "suc-is-nat-map" suc (MPi (MU mempty nat) mempty nat)
    ]
