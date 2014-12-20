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
import           Data.Set (Set)
import qualified Data.Set                   as DS
import qualified Data.List                  as DL
import qualified Data.ByteString.Lazy.Char8 as BL

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

results :: TVar (Set Integer)
results = unsafePerformIO $ newTVarIO (undefined)

dynCollatz :: Integer -> IO ()
dynCollatz n0 = do accum <- readTVarIO results
                   let (is, terminates) = coll (next n0) n0 accum 
                   if terminates then atomically $ writeTVar results $ recInsert is accum
                                 else putStrLn   $ "Loop @ " ++ show n0
  where
    coll :: Integer -> Integer -> Set Integer -> ([Integer], Bool)
    coll n i set = if n == i then ([n], False) else case DS.member n set of
      True -> ([n], True) ; False -> let (is, c) = coll (next n) i set in (n:is, c)

    next :: Integer -> Integer
    next n = case even n of False -> 3*n + 1 ; True -> n `div` 2

    recInsert :: Ord a => [a] -> Set a -> Set a
    recInsert is m = DL.foldr DS.insert m is

{-- main thread --}

main :: IO ()
main = do hSetBuffering stdout NoBuffering -- standard

          args <- getArgs
          let step  = (read (args !! 0)) :: Integer
          exsts1    <- doesFileExist "collatzMap.dat"
          exsts2    <- doesFileExist "collatzMap.prog"
          let exsts  = exsts1 && exsts2          
          if exsts then return () -- we assume these files exists. If they don't, we have to create them now
                   else do encodeFile "collatzMap.prog" (1                         ::     Integer)
                           encodeFile "collatzMap.dat"  ((DS.fromList [1]) :: Set Integer)

          savedSet <- decodeFile "collatzMap.dat"  :: IO (Set Integer)     -- read progress from disk
          prevsize <- decodeFile "collatzMap.prog" :: IO      Integer

          atomically $ writeTVar results savedSet
          let newSize = prevsize + step

          mapM_ forkIO [dynCollatz n | n <- [prevsize .. newSize]]

          removeFile "collatzMap.dat"  -- old files no longer needed!
          removeFile "collatzMap.prog"   

          waitForChildren -- Wait for all children to terminate

          newMap <- readTVarIO results
          
          encodeFile "collatzMap.prog" newSize
          encodeFile "collatzMap.dat"  newMap

          putStrLn $ "Finished calculating up to " ++  show newSize ++ " :)"
