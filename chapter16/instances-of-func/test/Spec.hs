import Lib
import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

functorComposeApplied :: (Num a, Eq (f a), Functor f) => f a -> Bool
functorComposeApplied = functorCompose (+ 1) (* 2)

identityIdentity :: Identity Int -> Bool
identityIdentity = functorIdentity

identityCompose :: Identity Int -> Bool
identityCompose = functorComposeApplied

pairIdentity :: Pair Int -> Bool
pairIdentity = functorIdentity

pairCompose :: Pair Int -> Bool
pairCompose = functorComposeApplied

twoIdentity :: Two Int Int -> Bool
twoIdentity = functorIdentity

twoCompose :: Two Int Int -> Bool
twoCompose = functorComposeApplied

threeIdentity :: Three Int Int Int -> Bool
threeIdentity = functorIdentity

threeCompose :: Three Int Int Int -> Bool
threeCompose = functorComposeApplied

three'Identity :: Three' Int Int -> Bool
three'Identity = functorIdentity

three'Compose :: Three' Int Int -> Bool
three'Compose = functorComposeApplied

fourIdentity :: Four Int Int Int Int -> Bool
fourIdentity = functorIdentity

fourCompose :: Four Int Int Int Int -> Bool
fourCompose = functorComposeApplied

four'Identity :: Four' Int Int -> Bool
four'Identity = functorIdentity

four'Compose :: Four' Int Int -> Bool
four'Compose = functorComposeApplied

main :: IO ()
main = do
  quickCheck identityIdentity
  quickCheck identityCompose
  quickCheck pairIdentity
  quickCheck pairCompose
  quickCheck twoIdentity
  quickCheck twoCompose
  quickCheck threeIdentity
  quickCheck threeCompose
  quickCheck three'Identity
  quickCheck three'Compose
  quickCheck fourIdentity
  quickCheck fourCompose
  quickCheck four'Identity
  quickCheck four'Compose
