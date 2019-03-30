module EarleyM.Pipe(Pipe,empty,firstWrite,firstRead,write,read,elems,readers) where

import Prelude hiding (read,elem)

data Pipe a r = Pipe { elems :: [a], readers :: [a -> r] }

empty :: Pipe a r
empty = Pipe { elems = [], readers = [] }

firstWrite :: a -> Pipe a r
firstWrite elem = Pipe { elems = [elem], readers = [] }

firstRead :: (a -> r) -> Pipe a r
firstRead k = Pipe { elems = [], readers = [k] }

write :: a -> Pipe a r -> (Pipe a r, [r])
write elem p = (Pipe (elem : elems p) (readers p) , rs)
  where rs = map (\k -> k elem) (readers p)

read :: (a -> r) -> Pipe a r -> (Pipe a r, [r])
read k p = (Pipe (elems p) (k : readers p) , rs)
  where rs = map k  (elems p)
