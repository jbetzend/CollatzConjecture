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
import qualified Data.List                  as DL
import qualified Data.Map.Strict            as DM
import qualified Data.ByteString.Lazy.Char8 as BL

-- TODO: Apparently, "Newtype" is coding convention
type TermMap = DM.Map Integer Bool

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

results :: TVar TermMap
results = unsafePerformIO $ newTVarIO (undefined)

dynCollatz :: Integer -> IO ()
dynCollatz n0 = do accum <- readTVarIO results
                   let (is, terminates) = coll (next n0) n0 accum 
                   if terminates then atomically $ writeTVar results $ recInsert is terminates accum
                                 else putStrLn   $ "Loop @ " ++ show n0
  where
    coll :: Integer -> Integer -> TermMap -> ([Integer], Bool)
    coll n i tmap = if n == i then ([n], False) else case DM.lookup n tmap of
      (Just b) -> ([n], b) ; Nothing -> let (is, c) = coll (next n) i tmap in (n:is, c)

    next :: Integer -> Integer
    next n = case even n of False -> 3*n + 1 ; True -> n `div` 2

    recInsert :: [Integer] -> Bool -> TermMap -> TermMap
    recInsert is b m = DL.foldr (\n -> DM.insert n b) m is

{-- main thread --}

main :: IO ()
main = do hSetBuffering stdout NoBuffering -- standard

          args <- getArgs
          let step  = (read (args !! 0)) :: Integer
          exsts1    <- doesFileExist "collatzMap.dat"
          exsts2    <- doesFileExist "collatzMap.prog"
          let exsts  = exsts1 && exsts2          
          if exsts then return () -- we assume these files exists. If they don't, we have to create them now
                   else do encodeFile "collatzMap.prog" (1                          :: Integer)
                           encodeFile "collatzMap.dat"  ((DM.fromList [(1, True)])  :: TermMap)

          savedMap <- decodeFile "collatzMap.dat"  :: IO TermMap -- read progress from disk
          prevsize <- decodeFile "collatzMap.prog" :: IO Integer

          atomically $ writeTVar results savedMap
          let newSize = prevsize + step

          mapM_ forkIO [dynCollatz n | n <- [prevsize .. newSize]]

          removeFile "collatzMap.dat"  -- old files no longer needed!
          removeFile "collatzMap.prog"   

          waitForChildren -- Wait for all children to terminate

          newMap <- readTVarIO results
          
          encodeFile "collatzMap.prog" newSize
          encodeFile "collatzMap.dat"  newMap

          putStrLn $ "Finished calculating up to " ++  show newSize ++ " :)"
