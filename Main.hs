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
children = unsafePerformIO (newMVar [])

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

dynCollatz :: TVar TermMap -> Integer -> IO ()
dynCollatz tm n0 = do accum <- readTVarIO tm
                      b <- coll n0 [] tm accum 
                      if b then return () else putStrLn $ "Loop: " ++ show n0
  where
    coll :: Integer -> [Integer] -> TVar TermMap -> TermMap -> IO Bool
    coll n is tvar tmap = case DM.lookup n tmap of
         (Just b) -> (atomically $ writeTVar tvar $ recInsert is b tmap) >> return b
         Nothing  -> if n `elem` is
           then putStrLn ("Loop detected! Element: " ++ show n) >> return False -- TODO: improve & sendmail
           else case even n of
                False -> coll ((3*n+1) `div` 2) (n:is) tvar tmap
                True  -> coll (n `div` 2)       (n:is) tvar tmap

    recInsert :: [Integer] -> Bool -> TermMap -> TermMap
    recInsert is b m = DL.foldr (\n -> DM.insert n b) m is

{-- main thread --}

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          args <- getArgs
          let inp  = (read (args !! 0)) :: Integer
          exsts1    <- doesFileExist "collatzMap.dat"
          exsts2    <- doesFileExist "collatzMap.prog"
          let exsts  = exsts1 && exsts2
          
          if exsts then return ()
                   else do encodeFile "collatzMap.prog" (1                          :: Integer)
                           encodeFile "collatzMap.dat"  ((DM.fromList [(1, True)])  :: TermMap)

          savedMap <- decodeFile "collatzMap.dat"  :: IO TermMap
          prevsize <- decodeFile "collatzMap.prog" :: IO Integer

          terminateMap <- atomically $ newTVar savedMap

          putStrLn "Now spawning..."

          mapM_ forkIO [dynCollatz terminateMap n | n <- [prevsize .. inp]] -- (should be prevsize + inp)

          removeFile "collatzMap.dat"  -- old files no longer needed!
          removeFile "collatzMap.prog"   

          putStrLn "Now waiting..."
          waitForChildren

          newMap <- readTVarIO terminateMap
          
          encodeFile "collatzMap.prog" inp
          encodeFile "collatzMap.dat"  newMap
          putStrLn "All done!"
