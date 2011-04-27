module Foo where
import Types
-- | Set a inside x to 1.
one :: State Bar ()
one = putf (x // a) 1
-- | Run this example.
runOne = runState one $ Bar (Foo 0 1) (Foo 2 3)
