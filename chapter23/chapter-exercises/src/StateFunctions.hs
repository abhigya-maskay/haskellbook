module StateFunctions where

import Moi

get :: Moi s s
get = Moi $ \ s -> (s,s)

put :: s -> Moi s ()
put s = Moi $ \ _ -> ((), s)

exec :: Moi s a -> s -> s
exec (Moi sa) s = let (_, s') = sa s in s'

eval :: Moi s a -> s -> a
eval (Moi sa) s = let (a', _) = sa s in a'

modify :: (s -> s) -> Moi s ()
modify modState = Moi $ \ s -> ((), modState s)
