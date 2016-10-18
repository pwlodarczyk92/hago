{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Map as Map
import qualified Data.Aeson as Aeson

import qualified Tests.Go.Fields as Fields
import qualified Tests.Go.Stones as Stones
import qualified Tests.Go.Position as Position
import qualified Tests.Go.Immortal as Immortal
import qualified Tests.Go.Atari as Atari
import qualified Data.ByteString.Lazy.Char8 as BS8
import Go.Core.Fields (F19)

import Control.Monad (forM_)
import Data.Maybe (fromJust, isJust)

fromSJust :: Maybe x -> x
fromSJust x = if isJust x then fromJust x else error "Tests"

main :: IO ()
main = do
    testCas "F19 tests" Fields.f19FieldInvariants
    testCas "Stones singleton tests" Stones.f19SingletonTest
    testCasList "Stones neighbors tests" Stones.f19NeighborsTest
    testCasList "Position kill tests   " Position.f19KillTest
    testMap "resources/test/cases-immortal.json" (show . Immortal.testf19)
    testMap "resources/test/cases-atari.json" (show . Atari.testf19)

testCas :: String -> Bool -> IO ()
testCas testname testresult = putStrLn $ testname  ++ ": " ++ show testresult

testCasList :: String -> [Bool] -> IO ()
testCasList testname testresults =
    forM_ (zip [0..] testresults)
        (\(i, tres) -> putStrLn $ testname ++ " " ++ show i ++ ": " ++ show tres)

testMap :: forall a . (Aeson.FromJSON a) => String -> (a -> String) -> IO ()
testMap filename testmake = do
    txt <- BS8.readFile filename
    case Aeson.eitherDecode txt of
        Left errmsg -> putStrLn errmsg
        Right parsed -> forM_ (Map.toList parsed) doTest
    where
        doTest :: (String, a) -> IO ()
        doTest (testName, testData) = putStrLn $ testName ++ ": " ++ testmake testData
