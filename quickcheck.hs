import Test.QuickCheck
import Data.List

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

-- Contains the same elements after grouping
prop_sameElements n s = (concat $ groupsOf n s) == s
