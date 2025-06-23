module DataSpec where

import Data
import Test.QuickCheck

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = mempty `mappend` a == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = a `mappend` mempty == a

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

type TrivIdentity = Trivial -> Bool

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

type IdentityIdentity = Identity String -> Bool

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

type TwoIdentity = Two String String -> Bool

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

type FourAssoc = Four String String String String -> Four String String String String -> Four String String String String -> Bool

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type BoolConjIdentity = BoolConj -> Bool

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type BoolDisjIdentity = BoolDisj -> Bool

type OrAssoc = Or String String -> Or String String -> Or String String -> Bool

type CombineAssoc = Fun String String -> Fun String String -> Fun String String -> String -> Property

prop_combineAssoc :: CombineAssoc
prop_combineAssoc (Fn f) (Fn f') (Fn f'') s = unCombine (a <> (b <> c)) s === unCombine ((a <> b) <> c) s
  where
    a = Combine f
    b = Combine f'
    c = Combine f''

type CombineIdentity = Fun String String -> String -> Bool

prop_combineLeftIdentity :: CombineIdentity
prop_combineLeftIdentity (Fn f) s = unCombine (mempty <> a) s == unCombine a s where
  a = Combine f

prop_combineRightIdentity :: CombineIdentity
prop_combineRightIdentity (Fn f) s = unCombine (a <> mempty) s == unCombine a s where
  a = Combine f

type CompAssoc = CombineAssoc

prop_compAssoc :: CompAssoc
prop_compAssoc (Fn f) (Fn f') (Fn f'') s = unComp (a <> (b <> c)) s === unComp ((a <> b) <> c) s
  where
    a = Comp f
    b = Comp f'
    c = Comp f''

type CompIdentity  = Fun String String -> String -> Bool

prop_compLeftIdentity :: CompIdentity
prop_compLeftIdentity (Fn f) s = unComp (mempty <> a) s == unComp a s where
  a = Comp f

prop_compRightIdentity :: CompIdentity
prop_compRightIdentity (Fn f) s = unComp (a <> mempty) s == unComp a s where
  a = Comp f

type ValidationAssoc = Validation String Int -> Validation String Int -> Validation String Int -> Bool

memFunc :: Mem Integer String
memFunc = Mem $ \ s -> ("hi", s + 1)


testData :: IO ()
testData = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: TrivIdentity)
  quickCheck (monoidRightIdentity :: TrivIdentity)

  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: IdentityIdentity)
  quickCheck (monoidRightIdentity :: IdentityIdentity)

  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: TwoIdentity)
  quickCheck (monoidRightIdentity :: TwoIdentity)

  quickCheck (semigroupAssoc :: ThreeAssoc)

  quickCheck (semigroupAssoc :: FourAssoc)

  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConjIdentity)
  quickCheck (monoidRightIdentity :: BoolConjIdentity)

  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisjIdentity)
  quickCheck (monoidRightIdentity :: BoolDisjIdentity)

  quickCheck (semigroupAssoc :: OrAssoc)

  quickCheck prop_combineAssoc
  quickCheck prop_combineLeftIdentity
  quickCheck prop_combineRightIdentity

  quickCheck prop_compAssoc
  quickCheck prop_compLeftIdentity
  quickCheck prop_compRightIdentity

  quickCheck (semigroupAssoc :: ValidationAssoc)

  let rmZero = runMem mempty 0
      rmleft = runMem (memFunc <> mempty) 0
      rmright = runMem (mempty <> memFunc) 0
  print rmleft
  print rmright
  print (rmZero :: (String, Int))
  print $ rmleft == runMem memFunc 0
  print $ rmright == runMem memFunc 0
