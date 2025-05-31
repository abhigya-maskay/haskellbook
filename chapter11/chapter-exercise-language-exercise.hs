-- [[file:chapter-exercises.org::*Language Exercise][Language Exercise:1]]
import Data.Char (toUpper)
import Data.List (intersperse, splitAt)

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x : xs) = (: xs) . toUpper $ x

capitalizeParagraph :: String -> String
capitalizeParagraph = foldr (++) "" . intersperse ". " . map capitalizeWord . breakParagraph
  where
    breakParagraph :: String -> [String]
    breakParagraph s = go s []
      where
        go "" acc = reverse acc
        go s acc =
          let paragraph = takeWhile (/= '.') s
              remainingString = drop 2 . dropWhile (/= '.') $ s
           in go remainingString (paragraph : acc)
-- Language Exercise:1 ends here
