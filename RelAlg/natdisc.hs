{-# OPTIONS -fglasgow-exts #-}

--------------------------------------------------------------------------------
{-| Module      : Discriminators for number types
    Copyright   : Fritz Henglein, 2009
    License     : All Rights Reserved
    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
--------------------------------------------------------------------------------
module RelAlg.NatDisc where

import Array (accumArray, elems)
import Control.Monad
import Data.Array.IO
import System.IO.Unsafe
import Data.IORef

{- Definitions for convenience and to eliminate directory passing -}

array :: Int -> e -> IO (IOArray Int e)
array size = newArray (0, size-1) 

get :: IOArray Int e -> Int -> IO e
get = readArray

set :: IOArray Int e -> Int -> e -> IO ()
set = writeArray

-- discNat size : Returns basic multiset discriminator with table of given size, preallocated and initialized.
--                Reuses same table.  In contrast to bucket sorting iterates over list of keys of added, not the the whole array 
--                ! The discriminator is order-preserving and reverse stable !
discNat :: Int -> [(Int, v)] -> [[v]]
discNat size =
  unsafePerformIO (
  do { table <- array size [] ;
       keys <- newIORef [] ;
       let discNat xs = unsafePerformIO (
            do { mapM_ (\(k, v) -> do { vs <- get table k;
                                        case vs of { [] -> do { ks <- readIORef keys;
                                                                writeIORef keys (k : ks); -- modifyIORef keys (k :) ;
                                                                set table k [v] } ;
                                                      _ -> set table k (v : vs) } })
                        xs;
                 ks <- readIORef keys ;
                 writeIORef keys [] ;
                 foldM (\vss k -> do { elems <- get table k ;
                                       set table k [] ;
                                       return (elems : vss) }) 
                        [] 
                        ks }
             )
       in return discNat } )  

-- discNat2 size: Just like discNat, but allocates a new mutable array for each call;
--                ! Practically useless because of extraordinary cost of allocating a new initialized array (IOArray)
discNat2 :: Int -> [(Int, v)] -> [[v]]
discNat2 size xs =
  unsafePerformIO (
  do { table <- array size [] ;
       keys <- newIORef [] ;
       mapM_ (\(k, v) -> do { vs <- get table k;
                              case vs of { [] -> do { modifyIORef keys (k :) ;
                                                      set table k [v] } ;
                                           _ -> set table k (v : vs) } } )
              xs;
       ks <- readIORef keys ;
       foldM (\vss k -> do { elems <- get table k ;
                             return (elems : vss) }) 
                        [] 
                        ks }
             )

{- sorting discriminators: Return keys in standard ascending order -}

-- sdiscNat size : Returns basic multiset discriminator with table of given size, preallocated and initialized.
--                 Maintains minimum and maximum key and returns result by iterating over indexes [minimum key .. maximum key]
--                 ! The discriminator i sorting and reverse stable !
sdiscNat :: Int -> [(Int, v)] -> [[v]]
sdiscNat size =
  unsafePerformIO (
  do { table <- array size [] ;
       let sdiscNat xs = unsafePerformIO (
            do { (minKey, maxKey) <- foldM (\(minKey, maxKey) (k, v) -> do { vs <- get table k ;
                                                                              set table k (v : vs) ;
                                                                              return (k `min` minKey, k `max` maxKey) } )
                                            (0, 0)
                                            xs;
                 foldM (\vss k -> do { elems <- get table k ;
                                       case elems of { [] -> return vss ;
                                                        _ -> do { set table k [] ;
                                                                  return (elems : vss) }}})
                        []
                        [maxKey, maxKey-1 .. minKey] } )
       in return sdiscNat } )


-- sdiscNat2 size : Just like sdiscNat, but allocates a new immutable array (IArray) for each call;
--                  ! Discriminator is sorting and reverse stable !
--                  ! In contrast to allocating a new mutable array this seems to be quite efficient !
sdiscNat2 :: Int -> [(Int, v)] -> [[v]]
sdiscNat2 n xs = [ vs | vs <- vss, not (null vs) ]
     where vss = elems (accumArray (\vs v -> v : vs) [] (0, n-1) xs)

xs0 :: [(Int, Float)]
xs0 = []
xs1 :: [(Int, String)]
xs1 = [(9, "AAA"), (4, "BBB"), (9, "CCC"), (0, "DDD")]
xs2 :: [(Int, Integer)]
xs2 = [(200, 2), (5000, 3), (100, 1), (200, 4), (100, 5), (5000, 6), (50000, 7), (5000, 8)]

main = (discNat16 xs0, discNat16 xs1, discNat16 xs2, 
        sdiscNat16 xs0, sdiscNat16 xs1, sdiscNat16 xs2)
  where discNat16 = discNat 65536
        sdiscNat16 = sdiscNat 65536