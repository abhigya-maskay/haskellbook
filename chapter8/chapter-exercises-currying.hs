-- [[file:chapter-exercises.org::Source][Source]]
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"
-- Source ends here

-- [[file:chapter-exercises.org::One][One]]
ans1 = appedCatty "woohoo!"
-- One ends here

-- [[file:chapter-exercises.org::Two][Two]]
ans2 = frappe "1"
-- Two ends here

-- [[file:chapter-exercises.org::Three][Three]]
ans3 = frappe (appedCatty "2")
-- Three ends here

-- [[file:chapter-exercises.org::Four][Four]]
ans4 = appedCatty (frappe "blue")
-- Four ends here

-- [[file:chapter-exercises.org::Five][Five]]
ans5 =
  cattyConny
    (frappe "pink")
    ( cattyConny
        "green"
        (appedCatty "blue")
    )
-- Five ends here

-- [[file:chapter-exercises.org::Six][Six]]
ans6 = cattyConny (flippy "Pugs" "are") "awesome"
-- Six ends here
