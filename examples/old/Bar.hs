module Bar where
import Types
-- | Set 'a' to 1.
one :: State Foo ()
one = modify $ \foo -> foo { a = 1 }
-- | Apply 'one' to 'x' and 'y'.
oneX :: State Bar ()
oneX = modify
  $ \bar -> bar { x = execState one $ x bar, y = execState one $ y bar }
-- | Run this example.
runOneX = runState oneX $ Bar (Foo 0 1) (Foo 2 3)
