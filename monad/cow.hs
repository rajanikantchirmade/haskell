

data Cow = Cow {
    name :: String
  , age  :: Int
  , weight :: Int
  } deriving (Eq, Show)


noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let n = name c
      w = weight c
  in if n == "Bess" && w > 499
    then Nothing
    else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow n a w =
  case noEmpty n of
    Nothing -> Nothing
    Just nammy ->
      case noNegative a of
        Nothing -> Nothing
        Just agey ->
          case noNegative w of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)


mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' n a w = do
  nammy <- noEmpty n
  agey  <- noNegative a
  weighty <- noNegative w
  weightCheck (Cow nammy agey weighty)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' n a w =
  noEmpty n >>=
    \nammy ->
      noNegative a >>=
        \agey ->
          noNegative w >>=
            \weighty ->
              weightCheck (Cow nammy agey weighty)
