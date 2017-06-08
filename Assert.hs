module Assert (assertEqual) where

-- Simplest testing framework :D
assert :: Eq a => a -> a -> Bool
assert expected actual = expected == actual

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO()
assertEqual message expected actual =
  let
    result = assert expected actual
  in
    if result == False
    then do
      putStrLn $ "[FAILED] " ++ message
      putStrLn $ "Expected --> " ++ (show expected)
      putStrLn $ "     Got --> " ++ (show actual)
    else
      putStrLn $ "[  OK  ] " ++ message
