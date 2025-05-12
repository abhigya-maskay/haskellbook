-- [[file:guard-duty.org::*Guard Duty][Guard Duty:1]]
pal xs
  | xs == reverse xs = True
  | otherwise = False
-- Guard Duty:1 ends here

-- [[file:guard-duty.org::*Guard Duty][Guard Duty:3]]
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1
-- Guard Duty:3 ends here
