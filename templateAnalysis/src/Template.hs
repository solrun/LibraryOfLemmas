module Template where
import Twee.Pretty
import qualified Data.Map.Strict as Map
import Data.List(nub,sortBy)
import Data.Ord(comparing)

data PropTemplate = Predicate TermTemplate
              | Equation Sign (TermTemplate, TermTemplate)
              | Implication [PropTemplate] PropTemplate
              | Negation PropTemplate
              | BImplication PropTemplate PropTemplate
              | Unknown
  deriving (Show, Eq, Ord)

data Sign = Equ | GE | GrT | LE | LeT
  deriving (Show, Eq, Ord)

-- We only care about term shape - nesting and which variables are the same (have the same Int label)
data TermTemplate = TVar Int | THole Int | TApp TermTemplate TermTemplate | TEmpty
  deriving (Show,Eq,Ord)

-- (name, stringrep, session, template)
type LabeledTemplate = (TemplateInfo, PropTemplate)
type TemplateInfo = (String, String, String)

class Template t where
  vsubst :: (Int -> Int) -> t -> t
  fsubst :: (Int -> Int) -> t -> t
  hasEmpty :: t -> Bool
  terms  :: t -> [TermTemplate]
  size   :: t -> Int
  depth  :: t -> Int
instance Template TermTemplate where
  vsubst s   (TVar k)     = TVar (s k)
  vsubst _ h@(THole _)    = h
  vsubst _    TEmpty      = TEmpty
  vsubst s   (TApp t1 t2) = TApp (vsubst s t1) (vsubst s t2)
  fsubst s   (THole k)    = THole (s k)
  fsubst _ v@(TVar _)     = v
  fsubst _    TEmpty      = TEmpty
  fsubst s   (TApp t1 t2) = TApp (fsubst s t1) (fsubst s t2)
  hasEmpty TEmpty       = True
  hasEmpty (TApp t1 t2) = hasEmpty t1 || hasEmpty t2
  hasEmpty _            = False
  terms t = [t]
  size (TVar _) = 1
  size (THole _) = 1
  size (TApp t1 t2) = size t1 + size t2
  size TEmpty = 1
  depth (TVar _) = 1
  depth (THole _) = 1
  depth (TApp t1 t2) = depth t1 `max` depth t2 + 1
  depth TEmpty = 1

instance Template PropTemplate where
  vsubst s (Predicate t)         = Predicate $ vsubst s t
  vsubst s (Equation sn (lt,rt)) = Equation sn (vsubst s lt, vsubst s rt)
  vsubst s (Implication ps p)    = Implication (map (vsubst s) ps) (vsubst s p)
  vsubst s (Negation p)          = Negation (vsubst s p)
  vsubst s (BImplication p1 p2)  = BImplication (vsubst s p1) (vsubst s p2)
  vsubst _ Unknown               = Unknown
  fsubst s (Predicate t)         = Predicate $ fsubst s t
  fsubst s (Equation sn (lt,rt)) = Equation sn (fsubst s lt, fsubst s rt)
  fsubst s (Implication ps p)    = Implication (map (fsubst s) ps) (fsubst s p)
  fsubst s (Negation p)          = Negation (fsubst s p)
  fsubst s (BImplication p1 p2)  = BImplication (fsubst s p1) (fsubst s p2)
  fsubst _ Unknown               = Unknown
  terms (Predicate t)           = [t]
  terms (Equation _ (lt,rt))    = [lt,rt]
  terms (Implication ps p)      = (concatMap terms (p:ps))
  terms (Negation p)            = terms p
  terms (BImplication p1 p2)    = (terms p1) ++ (terms p2)
  terms Unknown                 = [TEmpty]
  hasEmpty p = or $ map hasEmpty (terms p)
  size p = sum (map size $ terms p)
  depth p = maximum (map depth $ terms p)

measure :: Template t => t -> (Int,Int,Int,Int)
measure t = (size t, depth t, - (length $ vars t), - (length $ mvars t))

-- Normal Form
------------------
normalize :: PropTemplate -> PropTemplate
normalize = canonHoles . canonVars . orderparts where
  orderparts e@(Equation s (lt,rt)) = if measure lt <= measure rt then e else (Equation s (rt,lt))
  orderparts (Implication ps p)     = Implication (sortBy (comparing measure) ps ) (orderparts p)
  orderparts (Negation p)           = Negation (orderparts p)
  orderparts b@(BImplication p1 p2) = if measure p1 <= measure p2 then b else (BImplication p2 p1)
  orderparts x                      = x


---- possibly swap lhs and rhs of equations (need measure def for this?) - what about bimplications?
---- order conditions

-- do we even need this?
normalizeTerm :: TermTemplate -> TermTemplate
normalizeTerm = canonHoles . canonVars


---- canonicalize vars and holes
-- | Find all subterms of a term. Includes the term itself.
subterms :: TermTemplate -> [TermTemplate]
subterms t = t:properSubterms t

-- | Find all subterms of a term. Does not include the term itself.
properSubterms :: TermTemplate -> [TermTemplate]
properSubterms (TApp t u) = subterms t ++ subterms u
properSubterms _ = []

-- | All variable labels appearing in a `Template`, in order of appearance,
-- with duplicates included.
vars :: Template a => a -> [Int]
vars x = [ v | t <- terms x, TVar v <- subterms t ]

mvars :: Template a => a -> [Int]
mvars x = [ v | t <- terms x, THole v <- subterms t ]

canonVars :: Template t => t -> t
canonVars t = vsubst (\x -> Map.findWithDefault undefined x sub) t
  where sub = Map.fromList (zip (nub $ vars t) [0..])

canonHoles :: Template t => t -> t
canonHoles t = fsubst (\x -> Map.findWithDefault undefined x sub) t
  where sub = Map.fromList (zip (nub $ mvars t) [0..])

-- Pretty Printing
-------------------
instance Pretty Sign where
  pPrint Equ = text "="
  pPrint GE  = text ">="
  pPrint GrT = text ">"
  pPrint LE  = text "<="
  pPrint LeT = text "<"

instance Pretty TermTemplate where
  pPrint (TVar k)     = prettyName k "XYZWVUTS"
  pPrint (THole k)    = prettyName k "FGH"
  pPrint (TApp t1 t2) = pPrint t1 <+> (maybeParens (size t2 > 1) $ pPrint t2) -- maybe want to change this? Using TermStyle etc?
  pPrint (TEmpty)     = text "-_EMPTY_TERM_-"

prettyName :: Int -> String -> Doc
prettyName n names = text $
      names !! (n `mod` length names):
      case n `div` length names of
        0 -> ""
        m -> show (m+1)

instance Pretty PropTemplate where
  pPrint (Predicate t)         = pPrint t
  pPrint (Equation s (tl,tr))  = pPrint tl <+> pPrint s <+> pPrint tr
  pPrint (Implication lhs rhs) = hsep (punctuate (text " &") (map pPrint lhs))
                                  <+> text "==>" <+> pPrint rhs
  pPrint (Negation t)          = text "NOT" <+> (parens $ pPrint t)
  pPrint (BImplication t1 t2)  = pPrint t1 <+> text "<==>" <+> pPrint t2
  pPrint Unknown               = text "-_UNKNOWN_TEMPLATE_-"
