module Pipe(Pipe,empty,write,read,elems,readers) where

import Prelude hiding (read)

data Pipe a r = Pipe { elems :: [a], readers :: [a -> r] }

empty :: Pipe a r
empty = Pipe { elems = [], readers = [] }

write :: a -> Pipe a r -> (Pipe a r, [r])
write elem p = (Pipe (elem : elems p) (readers p) , rs)
  where rs = map (\k -> k elem) (readers p)

read :: (a -> r) -> Pipe a r -> (Pipe a r, [r])
read k p = (Pipe (elems p) (k : readers p) , rs)
  where rs = map k  (elems p)
