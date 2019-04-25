module Pretty where
import AST
import Data.List


class Plain a where
    plain :: a -> String

instance Plain a => Plain [a] where
    plain xs = intercalate "" (map plain xs)

instance Plain VisibilityRepr where
    plain Public = "public"
    plain Private = "private"

instance Plain AccessRepr where
    plain _ = "static"

instance Plain Id where
    plain (Id x) = x

instance Plain Unop where
    plain UnopMinus = "-"
    plain UnopNeg = "!"

instance Plain Binop where
    plain Gt = ">"
    plain Lt = "<"
    plain DEq = "=="
    plain Le = "<="
    plain Ge = ">="
    plain Ineq = "!="
    plain Conj = "&&"
    plain Disj = "||"
    plain Plus = "+"
    plain Minus = "-"
    plain Times = "*"
    plain Div = "/"

instance Plain LNum where
    plain (LNum x) = show x

instance Plain Stmt where
    plain (Block (StmtList l)) = "{" ++ (intercalate "" (map plain l)) ++ "}"
    plain (VarDecl tp id e) = plain tp ++ " " ++ plain id ++ "=" ++ plain e ++ ";"
    plain (VarAssn ref e) = plain ref ++ "=" ++ plain e ++ ";"
    plain (MethCall r l) = plain r ++ "(" ++ plain l ++ ");"
    plain (Ret e) = "return " ++ plain e ++ ";"
    plain (IfElse e s el) = "if(" ++ plain e ++ ")" ++ plain s ++
        (case el of
            Just s -> "else " ++ plain s
            Nothing -> "")
    plain (While e s) = "while(" ++ plain e ++ ")" ++ plain s

instance Plain Program where
    plain (Program pgm) = plain pgm

instance (Plain a, Plain b) => Plain (Either a b) where
    plain (Left a) = plain a
    plain (Right a) = plain a

instance (Plain a) => Plain (Maybe a) where
    plain (Just a) = plain a
    plain _ = ""

instance Plain Visibility where
    plain (Visibility r) = plain r

instance Plain Access where
    plain (Access x) = plain x

instance Plain Type where
    plain TInt = "int"
    plain TBool = "boolean"
    plain (TId id) = plain id
    plain TIntArray = "int  [   ]"
    plain (TIdArray id) = (plain id) ++ "[  ]"

instance Plain Comment where
    plain (OneLineComment str) = " //" ++ (show str) ++ "\n"
    plain (MultilineComment str) = " /*" ++ (show str) ++ "*/"
    plain _ = ""

instance Plain ParameterList where
    -- data ParameterList = ParameterList Type Id [(Type, Id)] deriving (Eq, Show, Generic)
    plain (ParameterList t i l') =
        let l = (t, i):l' in
        intercalate "," (map (\(a, b) -> plain a ++ " " ++ plain b) l)

instance Plain MethodDeclaration where
    -- Visibility Access Type Id (Maybe ParameterList) [Stmt] 
    plain (MethodDeclaration v a t i pl stmt) =
        let stms = intercalate "" (map plain stmt) in
        intercalate " " [plain v, plain a, case t of 
            Just x -> plain x
            Nothing -> "void"
            , plain i, "(" ++ plain pl ++ ")"] ++ "{" ++ stms ++ "}"

instance Plain FieldDeclaration where
    plain (FieldDeclaration visibility access tp id) =
        (intercalate " " [plain visibility, plain access, plain tp, plain id]) ++ ";"

instance Plain ClassDeclaration where
    plain (ClassDeclaration id xs) = "class " ++ (plain id) ++ "{" ++ (plain xs) ++ "}"

instance Plain Reference where
    plain (RId id) = plain id
    plain RThis = "this"
    plain (RNested r id) = (plain r) ++ "." ++ (plain id)

instance Plain IxReference where
    plain (IxReference r e) = plain r ++ "[" ++ plain e ++ "]"

instance Plain ArgumentList where
    plain (ArgumentList l r) = intercalate "," (map plain (l:r))

instance Plain Newable where
    -- data Newable = NId Id | NIntA Expr | NAnyA Expr deriving (Eq, Show, Generic)
    plain (NId id) = plain id ++ " (  )"
    plain (NIntA expr) = "int[" ++ plain expr ++ "]"
    plain (NAnyA id expr) = (plain id) ++ "[" ++ plain expr ++ "]"


instance Plain Expr where
    plain (RefExpr ref) = plain ref
    plain (IxExpr expr) = plain expr
    plain (CallExpr ref sl) = plain ref ++ "(" ++ plain sl ++ ")"
    plain (UnopExpr op expr) = plain op ++ plain expr
    plain (BinopExpr l op r) = plain l ++ plain op ++ plain r
    plain (Parens e) = "(" ++ plain e ++ ")"
    plain (IntLiteral l) = plain l
    plain (BLiteral True) = "true"
    plain (BLiteral False) = "false"
    plain (NewExpr newable) = "new " ++ plain newable
    plain (Commented cm expr) = plain cm ++ plain expr