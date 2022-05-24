module Template.Expand where

import Template

import Data.Maybe(isJust,catMaybes)
import Data.List(nub, subsequences, find, isSuffixOf)
import qualified Data.Map.Strict as Map
import Control.Monad(liftM)
import Debug.Trace

---------------------------------------------
-- Generalize templates
---------------------------------------------

-- TODO: add support for toggling expansion

--expandTemplate :: Int -> PropTemplate -> [(PropTemplate, Bool)]
--expandTemplate maxArity p = nub $ catMaybes $ concatMap (\x -> map (varReplace x) ["X","Y","Z",""]) $
--  concatMap (partialApp maxArity) $ nestApp p
--
---- partialApp maxArity p returns all possible expansions of p using partial application
---- with up to maxArity variables
--partialApp :: Int -> (PropTemplate, Bool) -> [(PropTemplate,Bool)]
--partialApp maxArity p = nub [foldl partialExpand p c | c <- combos]
--  where combos = crossProd [[(i,h)|i <- [0..maxArity]]| h <- (nub $ mvars $ fst p)]
---- TODO: Limit this expansion in some way?
--
---- Replace ?F with ?F X1 X2 ...
--partialExpand :: (PropTemplate, Bool) -> Int -> (PropTemplate, Bool)
--partialExpand (p,b) (k,h) |    -- hole_id h `elem`  ["X","Y","Z"]
--                            -- ||
--                               k <= ta = (p, b)
--                                 where ta = typeArity ht
--                                       ht = hole_ty h
--partialExpand (p,_) (k,h) | otherwise = (sprop (partialExpand' hname vnums lh,
--                                           partialExpand' hname vnums rh), True)
--  where (lh,rh) = sides p
--        k' = k - (typeArity (hole_ty h)) - 1
--        hname = hole_id h
--        fnum = freeVar [lh,rh]
--        vnums = [fnum..fnum+k']
--partialExpand' hid vns x@(THole k) | k == hid =
--                                              (THole $ MV {hole_id = hid ++ "partialExpand", hole_ty = typeVar})
--                                              :@: fvs
--                                          | otherwise = x
--          where fvs = [Var $ V typeVar vn | vn <- vns]
--partialExpand' hid vns (t1 :$: t2) = (partialExpand' hid vns t1) :$: (partialExpand' hid vns t2)
--partialExpand' _ _ x = x
--
--nestApp :: Prop (Term Constant) -> [(Prop (Term Constant), Bool)]
--nestApp p = (p, False) : [(appExpand p f, True) | f <- nub $ mvars p]
--              --[foldl appExpand p fs| fs <- subsequences (nub $ mvars p)]
--
---- Replace ?F with ?F1 applied to ?F2
--appExpand :: Prop (Term Constant) -> MetaVar -> Prop (Term Constant)
---- Assume that metavariable names "X", "Y", and "Z" are meant to represent constants
---- appExpand p m | hole_id m `elem` ["X","Y","Z"] = p
--appExpand p m | otherwise =  sprop (appExpand' h lh, appExpand' h rh)
--  where h = hole_id m
--        (lh,rh) = sides p
--        appExpand' mv (x@(Hole mv') :@: ts) =
--          if hole_id mv' == mv
--          then (Hole $ MV {hole_id = mv ++ "1", hole_ty = typeVar})
--               :$: ((Hole $ MV {hole_id = mv ++ "2", hole_ty = typeVar}) :@: ts')
--          else x :@: ts'
--          where ts' = map (appExpand' mv) ts
--        appExpand' mv (t1 :$: t2) = (appExpand' mv t1 :$: appExpand' mv t2)
--        appExpand' _ x = x
--
---- Replace a hole with a variable
--varReplace :: (Prop (Term Constant), Bool) -> String -> Maybe (Prop (Term Constant),Bool)
--varReplace (p,b) "" = Just (p,b)
--varReplace (p,b) m | m `elem` (map hole_id $ mvars p) = Just (sprop (replace m vn lh, replace m vn rh),b)
--    where vn = freeVar [lh,rh]
--          (lh,rh) = sides p
--          replace mname vnum x@(Hole mv) | hole_id mv == mname =
--                                          (Var $ V typeVar vnum)
--                                        | otherwise = x
--          replace mname vnum (t1 :$: t2) = (replace mname vnum t1) :$: (replace mname vnum t2)
--          replace _ _ t = t
--varReplace _ _ | otherwise = Nothing
--
--
--sides :: Prop a -> (a, a)
--sides (_ :=>: (sl :=: sr)) = (sl,sr)
--
--sprop :: (Term Constant, Term Constant) -> Prop (Term Constant)
--sprop (l,r) = Polymorphic.regeneralise ([] :=>: (l :=: r))
--
--testProps :: (Prop (Term Constant), Bool) -> [Constant] -> [Constant] -> [(Prop (Term Constant), Bool)]
--testProps (t,b) allcs currcs = zip (templateProps t allcs currcs) (repeat b)
