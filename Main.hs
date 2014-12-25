module Main where

{-- LOL, imports --}

import           System.IO
import           System.IO.Unsafe
import           System.Directory
import           System.Environment

import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM

import           Data.Binary
import           Data.Hashable
import           Data.HashSet (HashSet)
import qualified Data.HashSet               as HS
import qualified Data.List                  as DL
import qualified Data.ByteString.Lazy.Char8 as BL

{-- Type Foo and associates --}

type Results = (Integer, HashSet Integer)

unify :: Results -> Results
unify res@(n0, hs)
  | (HS.member (n0 + 1) hs) = unify (n0 + 1, HS.delete (n0 + 1) hs)
  | otherwise               = res

instance (Eq t, Hashable t, Binary t) => Binary (HashSet t) where
  put = (put . HS.toList)
  get = do hs <- get
           return $ HS.fromList hs

{-- Concurrency foo. Don't blame me for unsafePerformIO, docs for Control.Concurrent told me to do it! --}

children :: MVar [MVar ()]
children = unsafePerformIO $ newMVar []

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
       []   ->    return ()
       m:ms -> do putMVar children ms
                  takeMVar m
                  waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do mvar   <- newEmptyMVar
                  childs <- takeMVar children
                  putMVar children (mvar:childs)
                  forkFinally io (\_ -> putMVar mvar ())

{-- Collatz Foo --}

results :: TVar Results
results = unsafePerformIO $ newTVarIO (undefined)

dynCollatz :: Integer -> IO ()
dynCollatz n0 = do results_tvar <- readTVarIO results
                   let (is, terminates) = coll n0 n0 False results_tvar 
                   if terminates then atomically $ writeTVar results $ (fst results_tvar, recInsert is (snd results_tvar))
                                 else putStrLn   $ "Loop @ " ++ show n0
  where
    coll :: Integer -> Integer -> Bool -> Results -> ([Integer], Bool)
    coll n i visited res@(lim, set)
      | (n <= lim) = ([n], True)
      | (n == i)   = if visited then ([n], False)
                                else let (is, c) = coll (next n) i True res in (n:is, c)
      | otherwise  = if HS.member n set then ([n], True)
                                        else let (is, c) = coll (next n) i visited res in (n:is, c)

    next :: Integer -> Integer
    next n = case even n of False -> 3*n + 1 ; True -> n `div` 2

    recInsert :: (Ord a, Hashable a) => [a] -> HashSet a -> HashSet a
    recInsert is m = DL.foldr HS.insert m is

{-- main thread --}

main :: IO ()
main = do hSetBuffering stdout NoBuffering -- standard

          args <- getArgs
          let step  = (read (args !! 0)) :: Integer

          exsts <- doesFileExist "collatzMap.dat"
          if exsts then return () -- we assume these files exists. If they don't, we have to create them now
                   else encodeFile "collatzMap.dat" ((1, (HS.fromList [1])) :: Results)

          savedResults <- decodeFile "collatzMap.dat"  :: IO Results     -- read progress from disk
          atomically $ writeTVar results savedResults
          
          let prevSize = fst savedResults
          let newSize  = prevSize + step

          mapM_ (forkIO . dynCollatz) [prevSize .. newSize]

          removeFile "collatzMap.dat"  -- old files no longer needed!
          waitForChildren -- Wait for all children to terminate

          newSet <- readTVarIO results
          let final = unify newSet
          encodeFile "collatzMap.dat" final

          print final
          
          putStrLn $ "Finished calculating up to " ++ show newSize ++ " :)"
