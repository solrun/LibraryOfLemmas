{-# LANGUAGE TypeSynonymInstances #-}
module Template.Analyze where
import Template
import Template.Parse
import Twee.Pretty
import System.Directory
import System.FilePath
--import System.Directory.Recursive
--import QuickSpec.Internal.Prop
--import QuickSpec.Internal.Haskell
--import QuickSpec.Internal.Term
--import QuickSpec.Internal.Utils
--import QuickSpec.Internal.Type
--import QuickSpec.Internal.SchemeSpec.PropGen
--
--import qualified QuickSpec.Internal.Explore.Polymorphic as Polymorphic
--import QuickSpec.Internal(defaultTemplates)
--import Data.Lens.Light
import qualified Data.Map.Strict as Map
--import Data.List(isInfixOf,permutations,sortOn,nub)
import Data.Maybe(catMaybes)
import Data.List(sort,isSuffixOf)
import qualified Data.List as List
--import qualified System.IO.Strict as S
import Control.Exception

data TemplateStats = TemplateStats {
  count :: Int, -- how often does this template occur
  props :: [(String,String)], -- which properties does it match
  normalForm :: PropTemplate,
  isPredicate :: Bool,
  matchesLibTemplate :: [String]--,
  --matchesExpandedTemplate :: [(String, String)]--,
  --expansions :: [PropTemplate]
                                   }
instance Show TemplateStats where
  show (TemplateStats c _ n _ _) =
    (prettyShow n) ++ " count: " ++ (show c)

compileTemplateStats :: [LabeledTemplate] -> Map.Map String TemplateStats
compileTemplateStats ts = compileTemplateStats' Map.empty ts where
  compileTemplateStats' stats [] = stats
  compileTemplateStats' stats (t@((n,_,s),p):ps) = case Map.lookup (srep p) stats of
               Nothing -> compileTemplateStats' (Map.insert (srep p) (makeStats t) stats) ps
               Just _  -> compileTemplateStats' (Map.adjust (insertName (n,s)) (srep p) stats) ps
  srep = prettyShow . normalize
  insertName :: (String,String) -> TemplateStats -> TemplateStats
  insertName propname t = TemplateStats {count = count t + 1,
                                         props = props t ++ [propname],
                                         normalForm = normalForm t,
                                         isPredicate = isPredicate t,
                                         matchesLibTemplate = matchesLibTemplate t
                                        --,
                                        -- matchesExpandedTemplate = matchesExpandedTemplate t,
                                        -- expansions = expansions t
                                        }
  makeStats :: LabeledTemplate -> TemplateStats
  makeStats ((propname,_,src),prop) = TemplateStats {count = 1,
                                             props = [(propname,src)],
                                             normalForm = normalize prop,
                                             isPredicate = isPred prop,
                                             matchesLibTemplate = checkLibMatches prop
                                            --,
                                            -- matchesExpandedTemplate = checkExpandedLibMatches prop,
                                            -- expansions = map fst $ (expandTemplate 3) prop
                                            }
  isPred :: PropTemplate -> Bool
  isPred (Predicate _) = True
  isPred (Negation (Predicate _)) = True
  isPred _                        = False
--hasCondition :: Template -> Bool
--hasCondition ([] :=>: _) = False
--hasCondition _           = True
--
--countConditions :: [TemplateStats] -> (Int, Int)
--countConditions [] = (0,0)
--countConditions (t:ts) = (c+ct, n+cn)
--  where (c,n) = if hasCondition (normalForm t)
--                then (1, count t)
--                else (0,0)
--        (ct,cn) = countConditions ts
--
matches :: PropTemplate -> [PropTemplate] -> Bool
matches t ts = elem t (map normalize ts)
--matchesExpansions :: PropTemplate -> (TemplateStats, Int) -> Maybe Int
--matchesExpansions t (tstats, n) =
--  if t `elem` (map normalize (expansions tstats))
--  then Just n
--  else Nothing
--
--findExpansionMatches :: [(TemplateStats,Int)] -> [(Int,[Int])]
--findExpansionMatches ts = map (findExpansionMatches' ts) ts
--  where findExpansionMatches' tlist (t,k)= (k, kms (normalForm t) (expansionOptions (normalForm t) k tlist))
--        kms this others = catMaybes $ map (matchesExpansions this) others
--
--expansionOptions :: Template -> Int -> [(TemplateStats, Int)] -> [(TemplateStats, Int)]
--expansionOptions t k ts = filter (\(x,_) -> (size (normalForm x) < st) && (hasCondition (normalForm x) == hasCondition t) ) ((take (k-1) ts) ++ (drop k ts))
--  where st = size t
checkLibMatches :: PropTemplate -> [String]
checkLibMatches p = catMaybes $ map (checkMatch p) templateLib
  where checkMatch :: PropTemplate -> LabeledTemplate -> Maybe String
        checkMatch t ((ltname,_,_),lt) =
          if (normalize t == normalize lt)
          then Just ltname
          else Nothing

--checkExpandedLibMatches :: Template -> [(String, String)]
--checkExpandedLibMatches p = catMaybes $ map (expCheckMatch p) expandedLib
--  where expCheckMatch :: Template -> (String, [Template]) -> Maybe (String, String)
--        expCheckMatch t (ltname, lts) = case catMaybes $ map (checkMatch t) lts of
--          [] -> Nothing
--          (lt : _) -> Just (ltname, prettyShow lt)
--        checkMatch :: Template -> Template -> Maybe Template
--        checkMatch t lt =
--          if (normalizeTemplate t == normalizeTemplate lt)
--          then Just lt
--          else Nothing
templateLib :: [LabeledTemplate]
templateLib =   [(("identity"     ,"?F(X) = X"   ,"Default templates")
                  , Equation Equ (TApp (THole 1) (TVar 1), TVar 1))
                ,(("fix-point"    ,"?F(?X) = ?X" ,"Default templates")
                  , Equation Equ (TApp (THole 1) (TVar 1), THole 2))
                ,(("left-id-elem" ,"?F(?Y,X) = X","Default templates")
                  , Equation Equ (TApp (TApp (THole 1) (THole 2)) (TVar 1),TVar 1))
                ,(("right-id-elem","?F(X,?Y) = X","Default templates")
                  , Equation Equ (TApp (TApp (THole 1) (TVar 1)) (THole 2),TVar 1))
                ,(("cancel"       ,"?F(?G(X)) = ?F(X)", "Default templates")
                  , Equation Equ (TApp (THole 1) (TApp (THole 2) (TVar 1)), TApp (THole 1) (TVar 1)))
                ,(("commutative","?F(X,Y) = ?F(Y,X)","Default templates")
                  , Equation Equ (TApp (TApp (THole 1) (TVar 1)) (TVar 2), TApp (TApp (THole 1) (TVar 2)) (TVar 1)))
                ,(("op-commute","?F(?G(X)) = ?G(?F(X))", "Default templates")
                  , Equation Equ (TApp (THole 1) (TApp (THole 2) (TVar 1)),(TApp (THole 2) (TApp (THole 1) (TVar 1)))))
                --,("2-distributive"       ,"?F(?G(X,Y)) = ?G(?F(X),?F(Y))")
                --,("analogy-distributive" ,"?F(?G(X),?G(Y)) = ?G(?H(X,Y))")
                --,("associative-3"        ,"?F(?F(X,Y),Z) = ?F(X,?F(Y,Z))")
               ]
-- TODO: make parser for this style of template
--templateLib = map (\(name,trep) -> (name, parseT trep)) tlist
--  where tlist = [("identity"             ,"?F(X) = X")
--                ,("fix-point"            ,"?F(?X) = ?X")
--                ,("left-id-elem"         ,"?F(?Y,X) = X")
--                ,("right-id-elem"        ,"?F(X,?Y) = X")
--                ,("cancel"               ,"?F(?G(X)) = ?F(X)")
--                ,("commutative"          ,"?F(X,Y) = ?F(Y,X)")
--                ,("op-commute"           ,"?F(?G(X)) = ?G(?F(X))")
--                ,("2-distributive"       ,"?F(?G(X,Y)) = ?G(?F(X),?F(Y))")
--                ,("analogy-distributive" ,"?F(?G(X),?G(Y)) = ?G(?H(X,Y))")
--                ,("associative-3"        ,"?F(?F(X,Y),Z) = ?F(X,?F(Y,Z))")
--                ]

--expandedLib :: [(String, [Template])]
--expandedLib = map expandLib templateLib
--expandLib :: (String, Template) -> (String, [Template])
--expandLib (n,t) = (n, texpansions t)
--texpansions :: Template -> [Template]
--texpansions t = map fst $ (expandTemplate maxArity) t
--  where maxArity = 3
--
--
--
--matchExpansions :: [TemplateStats] -> (Int,[Int])
--matchExpansions = undefined
--main :: IO ()
--main = do
--  temps <- readFile "template-examples/templateOutput.txt"
--  let tm = templatesFromFile temps
--  putStrLn $ "all properties: " ++ (show $ length $ lines temps)
--  putStrLn $ "parsed properties: " ++ (show $ length tm)
--  let stats = compileTemplateStats tm
--  putStrLn $ "unique properties: " ++ (show $ Map.size stats)
--  let eqstats = Map.filter (\x -> isPredicate x == False) stats
--  putStrLn $ "unique equational properties: " ++ (show $ Map.size eqstats)
--  let orderedtemps = reverse $ sortOn (count . snd) (Map.toList eqstats)
--  --print $ head orderedtemps
--  let nconds = countConditions (map snd orderedtemps)
--  putStrLn $ "conditional templates & properties: " ++ (show nconds)
--  let ncets = filter (\t -> not $ hasCondition $ normalForm t) (map snd orderedtemps)
--  putStrLn $ "templates without conditions: " ++ (show $ length ncets)
--  let lncets = zip ncets [1..]
--  --putStrLn $ "number of properties covered by library templates: " ++ (show (sum $ map (\t -> count t) (filter (\x -> matchesLibTemplate x /= []) (map fst lncets))))
--  --putStrLn $ "number of properties covered by expansions of library templates: " ++ (show (sum $ map (\t -> count t) (filter (\x -> matchesExpandedTemplate x /= []) (map fst lncets))))
--
--  --let lts = zip (map snd orderedtemps) [1..]
--  --return ()
--  -- comparison to template library
--  -- compare 2-exp to 3-exp (1 more than before?) + count coverage
--  -- comparison to each other
--  putStrLn $ "Expansion matches: " ++ (show $ findExpansionMatches lncets)
--  putStrLn "Here come all the templates:"
--  mapM_ (\(t,n) -> putStrLn $ (show n) ++ "  " ++ (show t))lncets
--

parseSubSession :: FilePath -> String -> FilePath -> IO [LabeledTemplate]
parseSubSession src label f = do
  let subname = src ++ "/" ++ f
  dircheck <- doesDirectoryExist subname
  if dircheck
    then parseSessionTemplates subname (label ++ "/" ++ f)
    else
      if (isSuffixOf "templateEqs.txt" f) then do
        contents <- readFile subname
        Control.Exception.evaluate (length contents) -- hack to force file to close
        return $ (parseTemplates label) contents
      else return []

parseSessionTemplates :: FilePath -> String -> IO [LabeledTemplate]
parseSessionTemplates src label = do
  fs <- listDirectory src
  ts <- sequence $ map (parseSubSession src label) fs
  return $ concat ts

parseArchiveTemplates :: FilePath -> IO [LabeledTemplate]
parseArchiveTemplates src = do
  sessions <- listDirectory src
  ts <- sequence $ map (\sname -> parseSessionTemplates (src ++ "/" ++ sname) sname) sessions
  return $ concat ts
main :: IO ()
main = do
  let afpdata = "../Isabelle/Output/AFP/"
  templatedata <- parseArchiveTemplates afpdata
  let fulltemplates = filter (not . hasEmpty . snd ) templatedata
  let templatestats = compileTemplateStats fulltemplates
  let orderedTemplates = reverse $ List.sortOn (count . snd) $ Map.toList templatestats
  let templateFrequencies = map (count . snd) orderedTemplates
  return ()
  -- recursively parseTemplates in session directory
--  tte <- readFile "../Isabelle/TemplateOutput/TreetemplateEqs.txt"
--  let treeTemplates = parseTemplates tte
-- remove empty things
-- order by count

