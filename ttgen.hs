import Data.List
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec.Language as L

type VarType = String
data Formula = Variable VarType | Not Formula | And Formula Formula | Or Formula Formula
    deriving (Eq)

lexer = P.makeTokenParser (L.emptyDef { P.reservedOpNames = ["¬", "&", "|"] })
reservedOp = P.reservedOp lexer
identifier = P.identifier lexer
parens = P.parens lexer
whiteSpace = P.whiteSpace lexer

formula = buildExpressionParser table term <?> "formula"
table = [ [Prefix (reservedOp "¬" >> return Not)]
        , [Infix (reservedOp "&" >> return And) AssocLeft]
        , [Infix (reservedOp "|" >> return Or) AssocLeft]
        ]
term = parens formula <|> fmap Variable identifier

parseFormula f = parse formula "" f

vars :: Formula -> Set.Set String
vars (Variable v) = Set.singleton v
vars (Not v) = vars v
vars (And f g) = Set.union (vars f) (vars g)
vars (Or f g) = Set.union (vars f) (vars g)

evaluate :: Formula -> [(VarType, Bool)] -> Bool
evaluate (Variable a) vars = maybe (error $ "Invalid var " ++ show a) id $ lookup a vars
evaluate (Not f) vars = not $ evaluate f vars
evaluate (And f g) vars = evaluate f vars && evaluate g vars
evaluate (Or f g) vars = evaluate f vars || evaluate g vars

truthTable :: Formula -> [([(VarType, Bool)], Bool)]
truthTable formula = [(setting, evaluate formula setting) | setting <- allPossible . Set.toList . vars $ formula]


printTruthTable :: String -> IO ()
printTruthTable formulaString = do
    let formula = either (error "Parse failure") id $ parseFormula formulaString
        vs = Set.toList . vars $ formula
        header = ttHeader vs
    mapM_ putStrLn $ header : (ttSep $ length header) : (map ttRow . truthTable $ formula)

ttSep n = replicate n '-'

ttHeader vars = intercalate " " vars

ttRow (vars, result) = intercalate " " $ (map (toTF . snd) vars) ++ [" " ++ toTF result]

allPossible :: [a] -> [[(a,Bool)]]
allPossible [] = [[]]
allPossible (x:xs) = concat [ [(x,True):p] ++ [(x,False):p] | p <- allPossible xs ]

toTF True = "T"
toTF False = "F"
