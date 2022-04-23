import Test.QuickCheck
import TestSimulation

main :: IO ()
main = quickCheck prop_next
