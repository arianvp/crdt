import Algebra.Lattice

prop_joinSemiLatticeAssociative x y z =
  x \/ (y \/ z) == (x \/ y) \/ z

prop_joinSemiLatticeCommutative x y =
  x \/ y == y \/ x

prop_joinSemiLatticeIdempotent x =
  x \/ x == x

prop_boundedJoinSemiLatticeIdentity x =
  x \/ bottom == x

main :: IO ()
main = putStrLn "Test suite not yet implemented"
