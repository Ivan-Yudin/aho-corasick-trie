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

import Data.ListLike (nub,sort,uncons,groupBy)
import Data.MapLike 

import  Data.Map.Lazy (Map) 
import qualified  Data.Map.Lazy as M (intersectionWith, lookup,fromList,toList,empty,mapWithKey,null)
import qualified  Data.List as L (lookup,null)

---------------------------------------------------------------

data Maybe' a b = Just' a b | Nothing' deriving (Eq,Show,Read)

-------------------------------------------------------------
-- Classical instances for Maybe' 
------------------------------------------------------------

instance (Eq a) => Semigroup (Maybe' a b) where 
  Nothing' <> y  = y 
  x  <> _        = x

instance (Eq a ) => Monoid (Maybe' a b) where
  mempty = Nothing' 

instance (Eq a ) => MapLike (Maybe' a b) a b where 
  empty = Nothing' 

  null :: Maybe' a b -> Bool
  null Nothing' = True 
  null _        = False 

  singleton a b = Just' a b

  fromList [] =         Nothing' 
  fromList xx = uncurry Just'   $ last xx

  lookup x (Just' y b) | x == y = Just b 
  lookup _ _                    = Nothing  

  mapWithKey h (Just' a b) = Just' a (h a b) 
  mapWithKey _  _          = Nothing'

instance Functor (Maybe' a) where
  fmap :: (b -> c) -> (Maybe' a b -> Maybe' a c) 
  fmap f (Just' a b) = Just'    a (f b) 
  fmap f  Nothing'   = Nothing' 

-------------------------------------------------------------------
--  Applicative instances for @Maybe' a@ and @Map a@ 
-------------------------------------------------------------------

instance (Default a, Eq a) => Applicative (Maybe' a) where 
  pure :: b -> Maybe' a b
  pure b = Just' def b 

  liftA2 :: (b -> c -> d) -> (Maybe' a b) -> (Maybe' a c) -> Maybe' a d
  liftA2 h (Just' a b) (Just' a' c) | a == a' = Just' a (h b c) 
  liftA2 _ _ _ = Nothing' 

instance (Default a, Ord a) => Applicative (Map a) where 
  pure b = M.fromList [(def,b)] 
  
  liftA2 :: (b -> c -> d) -> (Map a b) -> (Map a c) -> Map a d
  liftA2 = M.intersectionWith

-------------------------------------------------------------------
  
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

match' root (Node out failure branch) [] = out 

match' root (Node out failure branch) xx@(x:xs)
  = case lookup x branch  of 
     Just  t                      -> out <> match' root t xs 
     Nothing  | Just t <- failure -> out <> match' root t xx 
              | otherwise         -> out <> match  root   xx  

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



tie :: (Eq out, Monoid out, Ord a, p~Maybe', t ~ Tree p out a
       , MapLike (p a t) a t, Functor (p a) ) => BinOpOn t 

tie  (Node outA _ continuation) rootB
    = Node outA Nothing $ fmap (prezip rootB) continuation 
  

--tie (Node outA _ (Just' a tA)) rootB = Node outA Nothing $ Just' a $ prezip tA  rootB 
                                                              -- ^insert vertex at the top at any case
   where 
    prezip t@(Node _     _ continuationB')
             (Node outA' _ continuationA') 
      = Node outA' Nothing $ flip mapWithKey continuationA'
        ( \a' -> case lookup a' continuationB' of 
                   Just tB' -> zipping tB'
                   Nothing  -> prezip  t )  


--    prezip t@(Node outB' failureB' (Just'  b' tB' ))
--             (Node outA' _         (Just'  a' tA' )) 
--               | a' == b'  = Node outA' failureB' $ Just'  a' $ zipping tB' tA' 
--               |otherwise  = Node outA' Nothing   $ Just'  a' $ prezip  t   tA'
  
--    prezip _ s = s 

    zipping t@(Node outB' _ _       )
              (Node outA' _ Nothing')
                       = Node (outB' <> outA') (Just t) Nothing'

    zipping t@(Node outB' failureB' (Just'  b' tB' ))
              (Node outA' _         (Just'  a' tA' )) 
              | a' == b'  = Node (outB' <> outA' ) failureB' $ Just'  a' $ zipping tB' tA' 
              |otherwise  = Node (outB' <> outA' ) (Just t)  $ Just'  a'
                              $ case h a' failureB' of 
                                  Nothing -> prezip rootB tA'
                                  Just tC -> zipping   tC tA' 
  
   -- h :: (Ord a) => a -> Maybe (Tree out a) -> Maybe (Tree out a )

    h _ Nothing = Nothing 
    h x (Just (Node _ f Nothing'))  = Nothing 
    h x (Just (Node _ f (Just'  y t ))) 
        | x == y    = h x f 
        | otherwise = Just t 

-----------------------------------------------------------------------
--            SOME TESTING STAFF  -------------------------------------
--            TO BE USED IN GHCI  -------------------------------------
-----------------------------------------------------------------------

 
t = prebuild1 (:[]) "ababcababcabcd" 
w = let s = tie t s in s
label (Node _  _        (Just' a t )) = a
next  (Node _  _        (Just' a t )) = t 
fails (Node _ (Just t )  _          ) = t 
h t k = foldr ($) t $ replicate k next


