main = putStrLn "done"

testRange = [153517..630395] :: [Int]

applyCriteria :: [Int] -> [Int]
applyCriteria = filter (both increasing twoAdjacentStrict)

both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
both f g x = f x && g x

increasing :: Int -> Bool
increasing n = all elt $ zip (init digits) (tail digits)
  where digits = show n
        elt (a,b) = a <= b

twoAdjacent :: Int -> Bool
twoAdjacent n = any eq $ zip (init digits) (tail digits)
  where digits = show n
        eq (a,b) = a == b

joinChar :: Char -> Char -> String
joinChar c1 c2 = c1 : c2 : ""

twoAdjacentStrict :: Int -> Bool
twoAdjacentStrict n = any cleanEq $ windows
  where digits = show n
        windowSize = length digits - 1
        leftpad = "a" ++ take (windowSize - 1) digits
        withLeft = zipWith joinChar (init digits) leftpad
        withRight = zipWith (:) (tail digits) withLeft
        withRightPad = zipWith (:) (drop 2 digits ++ "a") withRight
        windows = withRightPad
        cleanEq [a,b,c,d] = b == c && a /= b && d /= b
