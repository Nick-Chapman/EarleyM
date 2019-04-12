
-- | Type for "pipes", having multiple writers and readers.
--
-- Each element  (of type 'a') written to the pipe is "presented" to each reader (continuation) (of type @a -> r@).
--
-- Elements and readers can be adder in any order. When a new element is added, it is presented to the existing readers. When a new reader is adder, it is presented with the existing elements.
--
-- This type is used internally in the implementation of 'EarleyM', but it may be more widely useful.

module EarleyM.Pipe(
  Pipe, empty,
  write, read,
  firstWrite, firstRead,
  elems, readers
  ) where

import           Prelude hiding (elem, read)

-- | A "pipe" containing elements of type @a@, and readers of type @a -> r@.
data Pipe a r = Pipe {
  -- | Get the elements of a pipe.
  elems   :: [a],
  -- | Get the readers of a pipe.
  readers :: [a -> r]
  }

empty :: Pipe a r
-- | An empty pipe with no elements or readers.
empty = Pipe { elems = [], readers = [] }

-- | Create a pipe from a single element.
firstWrite :: a -> Pipe a r
firstWrite elem = Pipe { elems = [elem], readers = [] }

-- | Create a pipe from a single reader.
firstRead :: (a -> r) -> Pipe a r
firstRead k = Pipe { elems = [], readers = [k] }

-- | Write an element to a pipe, returning the new pipe and presentation results.
write :: a -> Pipe a r -> (Pipe a r, [r])
write elem p = (Pipe (elem : elems p) (readers p) , rs)
  where rs = map (\k -> k elem) (readers p)

-- | Attach a reader to a pipe, returning the new pipe and presentation results.
read :: (a -> r) -> Pipe a r -> (Pipe a r, [r])
read k p = (Pipe (elems p) (k : readers p) , rs)
  where rs = map k  (elems p)
