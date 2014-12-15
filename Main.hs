module Main where

import           System.IO
import qualified System.IO.Strict   as SIO
import           System.Directory
import           System.Environment

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import           Data.Binary
import qualified Data.List                  as DL
import qualified Data.Map.Strict            as DM
import qualified Data.ByteString.Lazy.Char8 as BL

type TermMap = DM.Map Integer Bool

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

          f <- SIO.readFile "collatzMap.dat"
          let savedMap = decode (BL.pack f) :: TermMap
          
          g <- SIO.readFile "collatzMap.prog"
          let prevsize = decode (BL.pack g) :: Integer

          terminateMap <- atomically $ newTVar savedMap
          mapM_ forkIO [dynCollatz terminateMap n | n <- [prevsize .. inp]]
          encodeFile "collatzMap.prog" inp
          newMap <- readTVarIO terminateMap
          encodeFile "collatzMap.dat"  newMap
          putStrLn "All done!"
