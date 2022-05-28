{-# LANGUAGE TypeSynonymInstances #-}
module Template.Analyze where
import Template
import Template.Parse
import Twee.Pretty
import System.Directory
import System.FilePath
import qualified Data.Map.Strict as Map
import Data.Maybe(catMaybes)
import Data.List(sort,isSuffixOf)
import qualified Data.List as List
import Control.Exception
import System.IO
data TemplateStats = TemplateStats {
  count :: Int,               -- how often does this template occur
  props :: [(String,String)], -- which properties does it match
  sources :: [String],        -- how many sources (AFP sessions) does it occur in?
  thys    :: [String],        -- how many theory files does it appear in?
  normalForm :: PropTemplate,
  isPredicate :: Bool
                                   }
instance Show TemplateStats where
  show (TemplateStats c _ s t n _ ) =
    (prettyShow n) ++ " count: " ++ (show c) ++ " sessions: " ++ (show $ length s) ++ " thy files: " ++ (show $ length t)

compileTemplateStats :: [LabeledTemplate] -> Map.Map String TemplateStats
compileTemplateStats ts = compileTemplateStats' Map.empty ts where
  compileTemplateStats' stats [] = stats
  compileTemplateStats' stats (t@((n,_,s),p):ps) = case Map.lookup (srep p) stats of
               Nothing -> compileTemplateStats' (Map.insert (srep p) (makeStats t) stats) ps
               Just _  -> compileTemplateStats' (Map.adjust (insertName (n,s)) (srep p) stats) ps
  srep = prettyShow . normalize
  insertName :: (String,String) -> TemplateStats -> TemplateStats
  insertName propname@(lname,sname) t = TemplateStats {count = count t + 1,
                                         props = props t ++ [propname],
                                         normalForm = normalForm t,
                                         sources = List.nub $ (sources t) ++ [sname],
                                         thys = List.nub $ (thys t) ++ [sname ++ "/" ++ (takeWhile (/='.') lname)],
                                         isPredicate = isPredicate t
                                        }
  makeStats :: LabeledTemplate -> TemplateStats
  makeStats ((propname,_,src),prop) = TemplateStats {count = 1,
                                             props = [(propname,src)],
                                             normalForm = normalize prop,
                                             sources = [src],
                                             thys = [src ++ "/" ++ (takeWhile (/='.') propname)],
                                             isPredicate = isPred prop
                                            }
  isPred :: PropTemplate -> Bool
  isPred (Predicate _) = True
  isPred (Negation (Predicate _)) = True
  isPred _                        = False

matches :: PropTemplate -> [PropTemplate] -> Bool
matches t ts = elem t (map normalize ts)

checkLibMatches :: PropTemplate -> [String]
checkLibMatches p = catMaybes $ map (checkMatch p) templateLib
  where checkMatch :: PropTemplate -> LabeledTemplate -> Maybe String
        checkMatch t ((ltname,_,_),lt) =
          if (normalize t == normalize lt)
          then Just ltname
          else Nothing

templateLib :: [LabeledTemplate]
templateLib =   [(("identity"     ,"?F(X) = X"   ,"Default templates")
                  , Equation Equ (TApp (THole 1) (TVar 1), TVar 1))
                ,(("fix-point"    ,"?F(?X) = ?X" ,"Default templates")
                  , Equation Equ (TApp (THole 1) (THole 2), THole 2))
                ,(("left-id-elem" ,"?F(?Y,X) = X","Default templates")
                  , Equation Equ (TApp (TApp (THole 1) (THole 2)) (TVar 1),TVar 1))
                ,(("right-id-elem","?F(X,?Y) = X","Default templates")
                  , Equation Equ (TApp (TApp (THole 1) (TVar 1)) (THole 2),TVar 1))
                ,(("cancel"       ,"?F(?G(X)) = ?F(X)", "Default templates")
                  , Equation Equ (TApp (THole 1) (TApp (THole 2) (TVar 1)), TApp (THole 1) (TVar 1)))
                ,(("commutative","?F(X,Y) = ?F(Y,X)","Default templates")
                  , Equation Equ (TApp (TApp (THole 1) (TVar 1)) (TVar 2), TApp (TApp (THole 1) (TVar 2)) (TVar 1)))
                ,(("commuting-functions","?F(?G(X)) = ?G(?F(X))", "Default templates")
                  , Equation Equ (TApp (THole 1) (TApp (THole 2) (TVar 1)),(TApp (THole 2) (TApp (THole 1) (TVar 1)))))
                ,(("distributivity","?F(?G(X,Y)) = ?G(?F(X),?F(Y))", "Default templates")
                  , Equation Equ (TApp (THole 1) (TApp (TApp (THole 2) (TVar 1)) (TVar 2)), TApp (TApp (THole 2) (TApp (THole 1) (TVar 1))) (TApp (THole 1) (TVar 2))))
                ,(("homomorphism" ,"?F(?G(X),?G(Y)) = ?G(?H(X,Y))", "Default templates")
                  ,Equation Equ (TApp (TApp (THole 1) (TApp (THole 2) (TVar 1))) (TApp (THole 2) (TVar 2))
                                ,(TApp (THole 2) (TApp (TApp (THole 3) (TVar 1)) (TVar 2)))))
                ,(("associativity","?F(?F(X,Y),Z) = ?F(X,?F(Y,Z))","Default templates")
                  ,Equation Equ (TApp (TApp (THole 1) (TApp (TApp (THole 1) (TVar 1)) (TVar 2))) (TVar 3)
                                ,TApp (TApp (THole 1) (TVar 1)) (TApp (TApp (THole 1) (TVar 2)) (TVar 3))))
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

findDefaultTemplates :: Map.Map String TemplateStats -> [(String, Maybe TemplateStats)]
findDefaultTemplates m = List.map (findTemplate m) templateLib where
  findTemplate :: Map.Map String TemplateStats -> LabeledTemplate -> (String, Maybe TemplateStats)
  findTemplate tmap ((tname,_,_),temp) = (tname, Map.lookup (prettyShow $ normalize temp) tmap)
main :: IO ()
main = do
  let afpdata = "../Isabelle/Output/AFP/"
  templatedata <- parseArchiveTemplates afpdata
  let fulltemplates = filter (not . hasEmpty . snd ) templatedata
  let templatestats = compileTemplateStats fulltemplates
  let orderedTemplates = reverse $ List.sortOn (count . snd) $ Map.toList templatestats
  let templateFrequencies = map (count . snd) orderedTemplates
  let maxthysTemplates = reverse $ List.sortOn (length . thys . snd) $ Map.toList templatestats
  let maxsessionsTemplates = reverse $ List.sortOn (length . sources . snd) $ Map.toList templatestats
  freqFile <- openFile "freq.txt" WriteMode
  hPrint freqFile templateFrequencies
  return ()

