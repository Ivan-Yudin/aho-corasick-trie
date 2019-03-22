{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Data.Tree.ACProfunctorTree where

import Prelude hiding (null, uncons,lookup)

import Control.Applicative ((<|>),liftA2,(<*>),pure)


import Data.Default.Class(Default,def)
import Data.Function (on)

import Data.Maybe (fromMaybe)
import Data.Maybe_ (Maybe'(..)) 

import Data.ListLike (nub,sort,uncons,groupBy)
import Data.MapLike

import  Data.Map.Lazy (Map)
import qualified  Data.Map.Lazy as M (keys,intersectionWith, lookup,fromList,toList,empty,mapWithKey,null)
import qualified  Data.List as L (lookup,null)


data Tree p out a = Node out                -- the output we produce if we reach the node
                     (Maybe (Tree p out a))    -- failure handling
                     (p a (Tree p out a)) -- continuation from the node driven by input


deriving instance (Eq   a, Eq   out) => Eq   (Tree Map    out a)
deriving instance (Eq   a, Eq   out) => Eq   (Tree Maybe' out a)
deriving instance (Show a, Show out) => Show (Tree Map    out a)
deriving instance (Show a, Show out) => Show (Tree Maybe' out a)

branch  (Node _   _ f) = f
failure (Node _   f _) = f




-- | Given  tree and a string collects outputs produced by each matchLing
-- substring. In the typical use @out~[a]@ and it equals to the matchLing
-- substring.
match :: (Monoid out, t ~ Tree p out a, MapLike (p a t) a t ) =>
         t  -> [a]  -> out

match _ [] = mempty

match root@(Node out _ branch) xx@(x:xs)
  = case lookup x branch of
     Just  t ->  out <> match' root t xs
     Nothing ->         match  root   xs

match' :: (Monoid out, t ~ Tree p out a, MapLike (p a t) a t ) =>
          t -> t  -> [a] -> out

match' root (Node out failure branch) []
  = case failure of 
      Nothing  -> out
      (Just t) -> out <> match' root t [] 

match' root (Node out failure branch) xx@(x:xs)
  = case lookup x branch  of
     Just  t                       -> out <> collectOut failure <> match' root t xs
     Nothing  | Just t  <- failure -> out <>                       match' root t xx
              | Nothing <- failure -> out <>                       match  root   xx

collectOut :: (Monoid out, t ~ Tree p out a, MapLike (p a t) a t ) =>
              Maybe t -> out
collectOut Nothing = mempty
collectOut (Just (Node out failure _)  )  = out <> collectOut failure
 

-- | The following function produces  tree that in conjunction with
-- match permits to find some matchLes in given sequences.
-- Namely it finds the longest matchLing prefix @p@ of a given string @s@,
-- outputs all matchLing prefixes of @p@, including @p@, and then resumes
-- with

prebuild1 :: (Ord a, Monoid out, Eq out,  t ~ Tree p out a
             , MapLike (p a t) a t ) =>
            ([a] -> out) -> [a] -> (Tree p out a)

prebuild1 f string = prebuild'  string (f string)
  where
    prebuild' :: (Ord a, Monoid out, Eq out, t~ Tree p out a
                 , MapLike (p a t) a t) =>
                  [a] -> out -> t

    prebuild' []     out =   Node out    Nothing   empty
    prebuild' (x:xs) out =   Node mempty Nothing $ singleton x
                                                 $ prebuild' xs out


prebuild :: (Ord a, Monoid out, Eq out, t ~ Tree p out a
            , MapLike (p a t) a t) =>
            ([a] -> out) -> [[a]] -> t
prebuild f strings =
   let strings' = nub $ sort strings
   in  prebuild' $ zip strings' (map f strings')
  where
--    prebuild' :: (Ord a, Monoid out, Eq out, t ~ Tree p out a) =>
--                 [( [a] , out )]  -> t

    prebuild' [] = Node mempty Nothing empty
    prebuild' pairs@( (x,out):pairs')
      | L.null x    =Node out    Nothing (createMap pairs')
      | otherwise =Node mempty Nothing (createMap pairs )
    createMap pairs = fromList $
              createKeyValuePair  <$>
              groupBy ( (==) `on` head.fst) pairs

--    createKeyValuePair ::(Ord a, Monoid out, Eq out) =>
 --                        [ ([a], out ) ] -> (a , Tree p out a  )

    createKeyValuePair pairs@( ( a:_ , _ ) : _)
      = ( a, prebuild' $ tailOnFst <$> pairs)

    tailOnFst :: ([a],out) -> ([a], out)
    tailOnFst (x,out) = (tail x , out)




type BinOpOn a = a -> a -> a



tie :: (Eq out, Monoid out, Ord a, t ~ Tree p out a
       , MapLike (p a t) a t, Functor (p a) ) => BinOpOn t

tie  (Node outA _ continuation) rootB
    = Node outA Nothing $ fmap (prezip rootB) continuation

   where
    prezip t@(Node _     _ continuationB')
             (Node outA' _ continuationA')
      = Node outA' Nothing $ flip mapWithKey continuationA'
        ( \a' -> case lookup a' continuationB' of
                   Just tB' -> zipping tB'
                   Nothing  -> prezip  t )


    zipping t@(Node _     _ continuationB')
              (Node outA' _ continuationA')
      = Node outA' (Just t)  continuationC
        where
          continuationC = merge hh continuationA' continuationB'

          hh _ (Just tA') (Just tB') = Just    $ zipping tB'   tA'
          hh _ (Just tA')  Nothing   = Just    $ prezip  rootB tA'
          hh _ _           _         = Nothing  



-----------------------------------------------------------------------
--            SOME TESTING STAFF  -------------------------------------
--            TO BE USED IN GHCI  -------------------------------------
-----------------------------------------------------------------------


t = prebuild1 (:[]) "ababcababcabcd" :: Tree Map [String] Char
tt = prebuild (:[]) ["abc","acd","cc"] :: Tree Map [String] Char
w = let s = tie t s in s
ww = tie tt ww
label (Node _  _        (Just' a t )) = a
labels (Node _  _        continuation) = M.keys continuation
next  (Node _  _        (Just' a t )) = t
nexts  (Node _  _         continuation) = map snd $ M.toList continuation
fails (Node _ (Just t )  _          ) = t
fails (Node _ Nothing _ )            = Node mempty Nothing empty
h t k = foldr ($) t $ replicate k next
h' t k = foldr (=<<) [t] $ replicate k nexts


