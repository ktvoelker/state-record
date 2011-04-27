module Bar where
import Types
-- | Set 'a' to 1.
one :: State Foo ()
one = putf a 1
-- | Apply 'one' to 'x'.
oneX :: State Bar ()
oneX = enter x one >> enter y one
-- | Run this example.
runOneX = runState oneX $ Bar (Foo 0 1) (Foo 2 3)
