import Lib (caesar, uncaesar, unvignere, vignere, Keyword(..), Message(..))
import Test.QuickCheck
  ( Gen,
    Property,
    arbitraryASCIIChar,
    chooseInt,
    forAll,
    listOf1,
    quickCheck,
    suchThat,
    elements
  )

genAlpha :: Gen String
genAlpha = listOf1 . elements $ ['a'..'z'] ++ ['A'..'Z']

prop_caesar :: Property
prop_caesar = forAll genAlpha $ \s ->
  forAll genPositiveInt $ \i ->
    caesarRoundtrip i s
  where
    genPositiveInt :: Gen Int
    genPositiveInt = chooseInt (0, 100)
    caesarRoundtrip :: Int -> String -> Bool
    caesarRoundtrip shifter s = (uncaesar shifter . caesar shifter $ s) == s

prop_vignere :: Property
prop_vignere = forAll genKeyword $ \k ->
  forAll genMessage $ \s ->
    vignereRoundtrip k s
  where
    genKeyword = Keyword <$> genAlpha
    genMessage = Message <$> genAlpha
    vignereRoundtrip :: Keyword -> Message -> Bool
    vignereRoundtrip k s = (unvignere k . vignere k $ s) == s

main :: IO ()
main = do
  quickCheck prop_caesar
  quickCheck prop_vignere
