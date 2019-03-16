
module Data.Tree.ACTree where 

import Prelude hiding (uncons)

import Control.Applicative ((<|>)) 

import Data.Function
import Data.ListLike (nub,sort,uncons,groupBy)
import  Data.Map.Lazy (Map) 
import qualified  Data.Map.Lazy as M (lookup,fromList,empty,mapWithKey,null)

data ACTree out a = ACNode out         -- the output we produce if we reach the node
                    (Maybe (ACTree out a)) -- failure handling
                    (Map a (ACTree out a)) -- continuations from the node driven by input
                    deriving (Show)

outAC     (ACNode out _ _) = out
forestAC  (ACNode _   _ f) = f
failureAC (ACNode _   f _) = f

-- | Given A-C tree and a string collects outputs produced by each matching
-- substring. In the typical use @out~[a]@ and it equals to the matching
-- substring. 
match :: (Monoid out, Ord a) => (ACTree out a) -> [a]  -> out
match tree string = match' tree tree string 

match' :: (Monoid out, Ord a) =>  (ACTree out a) -> (ACTree out a) -> [a] -> out
match' root (ACNode out failure forest) []        =  out 
match' root (ACNode out failure forest) xx@(x:xs) = 
  case M.lookup x forest of 
    (Just  t)            -> out <> match' root t    xs
    (Nothing)            ->
       case failure of 
        (Just  t)        -> out <> match' root t    xx
        (Nothing)        -> out <> match' root root xs

-- | The following function produces A-C tree that in conjunction with
-- match permits to find some matches in given sequences.  
-- Namely it finds the longest matching prefix @p@ of a given string @s@, 
-- outputs all matching prefixes of @p@, including @p@, and then resumes
-- with 

prebuild :: (Ord a, Monoid out, Eq out) => ([a] -> out) -> [[a]] -> (ACTree out a) 
prebuild f strings = prebuild' $ zip strings' (map f strings') 
  where 
    strings' = nub $ sort strings 
    prebuild' :: (Ord a, Monoid out, Eq out) =>[( [a] , out )]  -> (ACTree out a) 
    prebuild' [] =ACNode mempty Nothing M.empty
    prebuild' pairs@( (x,out):pairs') 
      | null x    =ACNode out    Nothing (createMap pairs')
      | otherwise =ACNode mempty Nothing (createMap pairs )
    createMap pairs = M.fromList $ 
              createKeyValuePair  <$>  
              groupBy ( (==) `on` head.fst) pairs
    
    createKeyValuePair ::(Ord a, Monoid out, Eq out) => [ ([a], out ) ] -> (a , ACTree out a  )
    createKeyValuePair pairs@( ( a:_ , _ ) : _) = ( a, prebuild' $ tailOnFst <$> pairs) 
    tailOnFst (x,out) = (tail x , out)


tie :: (Eq out, Monoid out, Ord a) => (ACTree out a) -> (ACTree out a ) ->  (ACTree out a) 

tie (ACNode out _     forest)
    (ACNode _   _ rootForest) = ACNode  out
                                        Nothing
                                       (fmap ( go Nothing) forest)
   where 
--     go :: (Eq out, Monoid out, Ord a) =>
--         --  (Map a (ACTree out a))      ->
--            Maybe (ACTree out a)       ->
--            ACTree out a               -> ACTree out a 
     go failure (ACNode out _ forest') =
       let 
          failureForest = case failure of 
            Nothing -> M.empty
            (Just t) -> let ACNode _ _ f = t in f  
          failureOut    = case failure of
            Nothing -> mempty
            (Just t) -> let ACNode o _ _ = t in o 
       in case M.null forest' of 
          True  -> ACNode (failureOut <> out) failure   M.empty
          False -> ACNode (failureOut <> out) failure $
                           M.mapWithKey (\key -> go   
                            ((M.lookup key failureForest) <|> 
                              -- ^if possible continue with current branch failures
                             (M.lookup key    rootForest)
                              -- ^alternatively, if possible, start to fail with top branch 
                             )  
                            ) forest' 


