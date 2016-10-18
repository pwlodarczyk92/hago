{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main where
import qualified Go.Server as Server
--getStones :: TestCase -> S.Stones F.F19
--getResult :: TestCase -> Set.Set F.F19
--getStones tcase = fromJust $ S.fromRawStones (tcase Map.! "points")
--getResult tcase = tcase Map.! "immortals"

--doTest :: TestCase -> IO ()
--doTest testcase = result where
--    immortals = getResult testcase
--    stones = getStones testcase
--    x :: Set.Set F.F19
--    x = Set.unions $ flip S.stones stones <$> (Set.toList . fst) (Im.getImmortals stones)
--    result = BS.putStr $ BS.pack $ show $ x == immortals

main :: IO ()
main = Server.main
