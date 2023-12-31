{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

module TransProp where

import "gf" PGF
import Prop   -- generated from GF
import TransPropFunctions
import TransLogicLaws
import Data.Tree

transfer :: Mode -> PGF -> Language -> PGF.Tree -> String
transfer m pgf la t = case m of
  MNone        -> lin id                                      -- no transformation
  -- MNone        -> showTreeP pgf la (fg t)                     -- Pieter: no transformation, but shows abstract syntax tree
  MMinimalize  -> lin (transform (minimalizeP . normalizeP))  -- interpretation functions
  MNormalize   -> lin (transform normalizeP)                  -- interpretation functions
  MOptimize    -> lin (transform optimizeP)                   -- the conversion rules of Ranta (2011) section 5.3
  MSimplify    -> simplifyP pgf la (fg t)                     -- Elze's formula simplification
 where
   lin :: (PGF.Tree -> PGF.Tree) -> String
   lin f = linearize pgf la (f t)

   transform :: (GProp -> GProp) -> (PGF.Tree -> PGF.Tree)
   transform f = gf . f . fg


-- Pieter: for optimising translation and printing linearisation in source language
formSen :: Mode -> PGF -> Language -> Language -> PGF.Tree -> String
formSen m pgf sl tl t = case m of
  MOptSen      -> optSenP pgf sl tl (fg t)
  MOptForm     -> optFormP pgf sl tl (fg t)

-- Pieter added MOptSen, MOptForm and MStatistics
data Mode = MNone | MOptimize | MMinimalize | MNormalize | MSimplify | MOptSen | MOptForm deriving Show    

-- the conversion rules of Ranta (2011) section 5.3 (core -> extended syntax)
optimizeP :: GProp -> GProp
optimizeP = optimize

optimize :: forall c. Prop.Tree c -> Prop.Tree c
optimize t = case t of
  -- Verb negation (e.g., "it is not the case that 1 is even" -> "1 is not even")
  GPNeg (GPAtom a) -> GPNegAtom $ optimize a -- Elze: added optimize (among other things for reflNegPred)

  -- Pieter: Exlusive disjunction
  -- Since the propositions inside the structure are atomic formulas, 
  -- there is no need for further optimisation of the inner propositions
  GPNeg (GPBimpl (GPAtom a1) (GPAtom a2)) -> GPExclusiveOr (GPAtom a1) (GPAtom a2)                      
  -- GPNeg (GPBimpl (GPNeg (GPAtom a1)) (GPAtom a2)) -> GPExclusiveOr (GPNegAtom a1) (GPAtom a2)           
  -- GPNeg (GPBimpl (GPAtom a1) (GPNeg (GPAtom a2))) -> GPExclusiveOr (GPAtom a1) (GPNegAtom a2)            
  -- GPNeg (GPBimpl (GPNeg (GPAtom a1)) (GPNeg (GPAtom a2))) -> GPExclusiveOr (GPNegAtom a1) (GPNegAtom a2)

  -- The optimisations below are commented out, as they are either seldomly used or result in
  -- more ambiguous sentences.
  -- -- Pieter: Or else
  -- GPConj GCOr p1 (GPImpl (GPNeg p2) q) | p1 == p2 -> GPOrElse (optimize p1) (optimize q)
  -- GPConj GCOr (GPAtom a1) (GPImpl (GPNegAtom a2) q) | a1 == a2 -> GPOrElse (GPAtom $ optimize a1) (optimize q) 

  -- -- Pieter: Only if
  -- GPImpl (GPNeg p) (GPNeg q) -> GPOnlyIf (optimize p) (optimize q)
  -- GPImpl (GPNegAtom a) (GPNeg q) -> GPOnlyIf (GPAtom $ optimize a) (optimize q)
  -- GPImpl (GPNeg p) (GPNegAtom a) -> GPOnlyIf (optimize p) (GPAtom $ optimize a)
  -- GPImpl (GPNegAtom a1) (GPNegAtom a2) -> GPOnlyIf (GPAtom $ optimize a1) (GPAtom $ optimize a2)

  -- -- Pieter: Unless
  -- GPImpl (GPNeg p) q -> GPUnless (optimize p) (optimize q)
  -- GPImpl (GPNegAtom a) q -> GPUnless (GPAtom $ optimize a) (optimize q)


  -- Elze: for existNeg & inSituExistNeg
  -- Existential negation ("it is not the case that there is no element x such that P" -> "there is no element x such that P")
  GPNeg (GPExist x p) | x `notElem` (freeVars p) -> GPNegExist x $ optimize p -- Elze: for existNeg
  GPNeg (GPExist x p) | x `elem` (freeVars p) -> inSituWithoutKind GPNegExist GNothing_IExist x $ optimize p

  -- Aggregation and flattening (e.g., "2 is even and 4 is even" -> "2 and 4 are even")
  GPConj co p q -> aggregate co $ optimize $ mergeConj co p q
  GPConjs co ps -> aggregate co $ optimize ps

   -- Reflexivization (e.g., "2 is equal to 2" -> "2 is equal to itself")
  GAPred2 f x y | x == y -> GAPredRefl f x  ---- and no insitu quant in x
  GAPredColl f (GListInd xs) | length xs == 2 && (xs !! 0) == (xs !! 1) -> GAPredRefl f (xs !! 0) --Elze: for reflNegPred

  -- Partial application (e.g., "Parallel(a,b)" -> "Parallel_to_b(a)")
  GAPred2 f x y -> GAPred1 (GPartPred f y) x

  -- Move kind out of body (e.g., "for all x, if x is a number then P" -> "for all numbers x, P")
  GPUniv x (GPImpl (GPAtom (GAKind k (GIVar y))) p) | y == x ->
    let (km,pm) = getKindMod x k p in optimize $ GPUnivs (GListVar [x]) km pm
  GPExist x (GPConj GCAnd (GPAtom (GAKind k (GIVar y))) p) | y == x ->
    optimize $ GPExists (GListVar [x]) k p

  -- Elze: for inSituWithoutKind
  -- In-situ quantification without kind predicate, e.g., "for all x , x is even" -> "everything is even"
  GPUniv x p | x `elem` (freeVars p) -> inSituWithoutKind GPUniv GEverything_IUniv x $ optimize p
  GPExist x p | x `elem` (freeVars p) -> inSituWithoutKind GPExist GSomething_IExist x $ optimize p
  GPNegExist x p | x `elem` (freeVars p) -> inSituWithoutKind GPNegExist GNothing_IExist x $ optimize p

  -- In-situ quantification (e.g., "for all numbers x, x is even" -> "every number is even")
  GPUnivs  (GListVar [x]) k p -> inSitu GPUnivs  (GIUniv k)  k x $ optimize p
  GPExists (GListVar [x]) k p -> inSitu GPExists (GIExist k) k x $ optimize p

  -- Apply optimize to all subtrees of t and combine them back
  _ -> composOp optimize t

-- Flattening
-- assumes everything inside is binary conjunctions
mergeConj :: GConj -> GProp -> GProp -> GListProp
mergeConj co p q = GListProp (getConj p ++ getConj q)
 where
  getConj :: GProp -> [GProp]
  getConj p = case p of
    GPConj ko p1 p2 | ko == co -> getConj p1 ++ getConj p2
    _ -> [p]

-- Aggregation
aggregate :: GConj -> GListProp -> GProp
aggregate co p@(GListProp ps) = case getPred1s ps of
  -- Subject-sharing
  Just (fs,xs@(x:_)) | all (== x) xs -> GPAtom (GAPred1 (GConjPred1 co (GListPred1 fs)) x)

  -- Predicate-sharing
  Just (fs@(f:_),xs) | all (== f) fs -> GPAtom (GAPred1 f (GConjInd co (GListInd xs)))
  _ -> case getPred2s ps of    -- Elze: for aggregPred2
    -- Subject-sharing
    Just (fs@(f:_),xs@(x:(_))) | all (== f) fs && all (== (fst x)) (map fst xs) -> GPAtom (GAPred2 f (fst x) (GConjInd co (GListInd (map snd xs))))
    -- Object-sharing
    Just (fs@(f:_),xs@(x:(_))) | all (== f) fs && all (== (snd x)) (map snd xs) -> GPAtom (GAPred2 f (GConjInd co (GListInd (map fst xs))) (snd x))
    _ -> GPConjs co p

-- In-situ quantification
inSitu :: (GListVar -> GKind -> GProp -> GProp) -> GInd -> GKind -> GVar -> GProp -> GProp
inSitu quant qp k x b = case b of
  GPAtom (GAPred1 (GPartPred f y) z)              -> inSitu quant qp k x (GPAtom (GAPred2 f z y))
  GPAtom (GAPred1 f y)   | y == vx                -> GPAtom (GAPred1 f qp)
  GPAtom (GAPred1 f (GIFun1 h y))   | y == vx     -> GPAtom (GAPred1 f (GIFun1 h qp))
  GPAtom (GAKind  f y)   | y == vx                -> GPAtom (GAKind f qp)
  GPAtom (GAPredRefl f z)| z == vx                -> GPAtom (GAPredRefl f qp)
  GPAtom (GAPred2 f z y) | y == vx && notFree x z -> GPAtom (GAPred2 f z qp)
  GPAtom (GAPred2 f z y) | z == vx && notFree x y -> GPAtom (GAPred2 f qp y)
  _ -> quant (GListVar [x]) k b
 where
  vx = GIVar x

-- In-situ quantification without kind predicate (Elze: for inSituWithoutKind)
inSituWithoutKind :: (GVar -> GProp -> GProp) -> GInd -> GVar -> GProp -> GProp
inSituWithoutKind quant qp x b = case b of
  GPAtom (GAPred1 (GPartPred f y) z)              -> inSituWithoutKind quant qp x (GPAtom (GAPred2 f z y))
  GPAtom (GAPred1 f y)   | y == vx                -> GPAtom (GAPred1 f qp)
  GPAtom (GAPred1 f (GIFun1 h y))   | y == vx     -> GPAtom (GAPred1 f (GIFun1 h qp))
  GPAtom (GAKind  f y)   | y == vx                -> GPAtom (GAKind f qp)
  GPAtom (GAPredRefl f z)| z == vx                -> GPAtom (GAPredRefl f qp)
  GPAtom (GAPred2 f z y) | y == vx && notFree x z -> GPAtom (GAPred2 f z qp)
  GPAtom (GAPred2 f z y) | z == vx && notFree x y -> GPAtom (GAPred2 f qp y)
  _ -> quant x b
 where
  vx = GIVar x

-- Modification
getKindMod :: GVar -> GKind -> GProp -> (GKind,GProp)
getKindMod x k p = case p of
  GPImpl (GPAtom (GAPred1 q y)) p' | y == GIVar x -> (GModKind k q, p')
  _ -> (k,p)

-- Interpretation functions (extended -> core syntax)
-- this applies to a normalized Prop and makes it binary
minimalizeP :: GProp -> GProp
minimalizeP p = case p of
  -- List conjunction interpreted as folding with binary conjunction
  GPConjs co (GListProp ps) -> foldl1 (GPConj co) (map minimalizeP ps)

  -- Move kind K to body and interpret modified predicates as conjunctions
  GPUnivs (GListVar xs) k b ->
    foldl (flip GPUniv)
          (GPImpl (foldl1 (GPConj GCAnd) [minKind k x | x <- xs]) (minimalizeP b))
          xs
  GPExists (GListVar xs) k b ->
    foldl (flip GPExist)
          (GPConj GCAnd (foldl1 (GPConj GCAnd) [minKind k x | x <- xs]) (minimalizeP b))
          xs
  _ -> p
 where
  -- Modified predicate interpreted as conjunction
  minKind :: GKind -> GVar -> GProp
  minKind k x = case k of
    GModKind b f -> GPConj GCAnd (minKind b x) (GPAtom (GAPred1 f (GIVar x)))
    _ -> (GPAtom (GAKind k (GIVar x)))


normalizeP :: GProp -> GProp
normalizeP = iProp

iProp :: GProp -> Prop
iProp p = case p of
  -- Atomic predicates interpreted as themselves
  GPNegAtom a -> GPNeg (iAtom a)
  GPAtom a -> iAtom a

  -- Subject negation as existential negation
  GPNegExist x b -> GPNeg (GPExist x (iProp b)) -- Elze: for existNeg

  -- List conjunction as folding with binary conjunction
  GPConjs co (GListProp ps) -> GPConjs co (GListProp (map iProp ps))

  -- Quantification interpreted as itself
  GPUnivs xs k b -> GPUnivs xs k (iProp b)
  GPExists xs k b -> GPExists xs k (iProp b)
  _ -> p

iAtom :: GAtom -> Prop
iAtom a = case a of
  -- Atomic predicates and simple kind predicates interpreted as themselves
  GAKind  f x     -> iInd x (\i -> GPAtom (GAKind f i))

  -- 2-place predication requires lambda abstraction
  GAPred1 f x     -> iInd x (iPred1 f)
  GAPred2 f x y   -> iInd x (\u -> iInd y (\v -> iPred2 f u v))

  -- Reflexives expand to repeated application
  GAPredRefl f x  -> iInd x (\u -> iPred2 f u u)
  _               -> GPAtom a  -- Elze: fixed 'non-exhaustive patterns in case'

iPred1 :: GPred1 -> Ind -> Prop
iPred1 f i = case f of
  -- Predicate conjunction interpreted as proposition conjunction
  GConjPred1 co (GListPred1 fs) -> GPConjs co (GListProp [iPred1 g i | g <- fs])
  -- Partial application interpreted as 2-place predicate
  GPartPred f y -> iPred2 f i y
  _ -> GPAtom (GAPred1 f i)

iPred2 :: GPred2 -> Ind -> Ind -> Prop
iPred2 f i j = case f of
  _ -> GPAtom (GAPred2 f i j)

iInd :: GInd -> (Ind -> Prop) -> Prop
iInd q f = case q of
  -- Propositional function application on any or a single arbitary element
  -- of kind K in the domain
  GIUniv  k -> let x = newVar 1 in GPUnivs  (GListVar [x]) k (f (GIVar x))
  GIExist k -> let x = newVar 2 in GPExists (GListVar [x]) k (f (GIVar x))

  -- Elze: for inSituWithoutKind
  GEverything_IUniv -> let x = newVar 3 in GPUniv  x (f (GIVar x))
  GSomething_IExist -> let x = newVar 4 in GPExist x (f (GIVar x))
  GNothing_IExist   -> let x = newVar 5 in GPNegExist x (f (GIVar x))

  GIFun1 g r -> iInd r (\x -> f (GIFun1 g x))
  GIFun2 g r s -> iInd r (\x -> iInd s (\y -> f (GIFun2 g x y)))
  GIFunC g (GListInd rs) -> wind rs (\x y -> f (GIFun2 g x y)) where
    wind qs h = case qs of
      [r,s] -> iInd r (\x -> iInd s (\y -> h x y))
      r :ss -> iInd r (\x -> wind ss (\y z -> h (GIFun2 g x y) z))

  -- Individual conjunction interpreted as propositional function conjunction
  GConjInd co (GListInd rs) -> wind rs (\x y -> GPConj co (f x) (f y)) where
    wind qs h = case qs of
      [r,s] -> iInd r (\x -> iInd s (\y -> h x y))
      r :ss -> iInd r (\x -> wind ss (\y z -> GPConj co (GPConj co (f x) (f y)) (f z)))
  GIVar _  -> f q
  GIInt _  -> f q


type Prop = GProp
type Ind = GInd


----------------------------------------------------------------------------------------
-- Simplification by Elze

-- Simplify a proposition given the target language (the chosen simplification
-- sequence is based on the length of the output translation) 
-- The output string contains the resulting abstract syntax tree + the linearization
simplifyP :: PGF -> Language -> GProp -> String
simplifyP = simplify

simplify :: PGF -> Language -> GProp -> String
simplify pgf la p = (showExpr [] ((gf . snd) ((flatten t) !! i))) ++ ";" ++ 
  s ++ ";" ++ wb ++ show (wordCount s)
   where
     lin = linearize pgf la

     -- Build tree of possible simplifying operations,
     -- where each node is a tuple: (depth in tree, (simplified) proposition)
     buildNode n =
       if fst n == 5 then (n, [])   -- if max depth of tree is reached, terminate
         else (n, [((fst n) + 1, law (snd n)) | law <- (logicLaws ++ bimplLaws), law (snd n) /= snd n])
     t = unfoldTree buildNode (0, p)

     flatTree = flatten t
     (s, i) = shortestSentence (map (lin . gf . optimizeP . snd) flatTree)

     wbList = map (isWB . snd) flatTree
     wb_bool = wbList !! i
     wb =
       if wb_bool then "WB;"
       else "NWB;"


----------------------------------------------------------------------------------------
-- Pieter's additions

-- Print the logical structure of a formula, and its baseline translation
showTreeP :: PGF -> Language -> GProp -> String
showTreeP = showTree

showTree :: PGF -> Language -> GProp -> String
showTree pgf la p = showExpr [] (gf p) ++ ", " ++ s
   where
     lin = linearize pgf la
     s = head [(lin . gf) p]

-- Simplify a proposition given the target language (the chosen simplification
-- sequence is based on the length of the output translation and its well-behavedness) 
-- The output string contains the linearisation in the source language and in the target language
optSenP:: PGF -> Language -> Language -> GProp -> String
optSenP = optSen

optSen:: PGF -> Language -> Language -> GProp -> String
optSen pgf sl tl p = for f ++ ";" ++ s ++ ";" ++ wb ++ show (wordCount s)
   where
     f = gf (snd (flatTree !! i))
     lin = linearize pgf tl
     for = linearize pgf sl
     
     -- Build tree of possible simplifying operations,
     -- where each node is a tuple: (depth in tree, (simplified) proposition)
     buildNode n =
       if fst n == 5 then (n, [])   -- if max depth of tree is reached, terminate
         else (n, [((fst n) + 1, law (snd n)) | law <- (logicLaws ++ bimplLaws), law (snd n) /= snd n])
     t = unfoldTree buildNode (0, p)
     flatTree = flatten t
     --
     wbList = map (isWB . snd) flatTree
     wb =
       if wbList !! i then "WB;"
       else "NWB;"
    
     sentenceList = map (lin . gf . optimizeP . snd) flatTree
     senWB = zip sentenceList wbList
     (s, i) = shortestWB senWB
     
    --  (s, i) = shortestSentence (map (lin . gf . optimizeP . snd) (flatten t))


-- Simplify a proposition given the source language (the chosen simplification
-- sequence is based on the length of the linearisation in the source language) 
-- The output string contains the linearisation in the source language and in the target language
optFormP:: PGF -> Language -> Language -> GProp -> String
optFormP = optForm

optForm:: PGF -> Language -> Language -> GProp -> String
optForm pgf sl tl p =  f ++ ";" ++ lin s ++ ";" ++ show (wordCount (lin s))
   where
     s = gf ((optimizeP . snd) ((flatten t) !! i))
     lin = linearize pgf tl
     for = linearize pgf sl
     
     -- Build tree of possible simplifying operations,
     -- where each node is a tuple: (depth in tree, (simplified) proposition)
     buildNode n =
       if fst n == 5 then (n, [])   -- if max depth of tree is reached, terminate
         else (n, [((fst n) + 1, law (snd n)) | law <- (logicLaws ++ bimplLaws), law (snd n) /= snd n])
     t = unfoldTree buildNode (0, p)
     (f, i) = shortestFormula (map (for . gf . snd) (flatten t))
