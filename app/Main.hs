{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GHC.Exts (fromList)
import Data.SBV
import Data.SBV.Control
import Data.SBV.List ((.!!))
import Data.SBV.Tools.BoundedList
import qualified Data.SBV.List as L

example1 :: Symbolic [(Integer, Integer)]
example1 = do a :: SBV Integer <- exists "a"
              b :: SBV Integer <- exists "b"
              as <- return $ fromList [0,1,4,2,1,2,3,0]
              constrain $ a `L.elem` as
              constrain $ (a `sMod` 2) .== 0
              constrain $ b `L.elem` as
              constrain $ b .>= a

              let run accum = do
                      cs <- checkSat
                      case cs of
                        Unk   -> error "Too bad, solver said unknown.." -- Won't happen
                        Unsat -> return accum
                        Sat   -> do av <- getValue a
                                    bv <- getValue b
                                    constrain $ (a ./= literal av) .|| (b ./= literal bv)
                                    run ((av, bv):accum)

              query $ do run []

example2 :: Symbolic [[Integer]]
example2 = do as :: SList Integer <- exists "as"
              constrain $ L.length as .== 4
              constrain $ ball 4 (.< 2) as
              constrain $ ball 4 (.>= 0) as

              let run accum = do
                      cs <- checkSat
                      case cs of
                        Unk   -> error "Too bad, solver said unknown.." -- Won't happen
                        Unsat -> return accum
                        Sat   -> do asv <- getValue as
                                    constrain $ as ./= fromList asv
                                    run (asv:accum)

              query $ do run []



main = do
  result <- runSMTWith z3 example2
  putStrLn $ show result

