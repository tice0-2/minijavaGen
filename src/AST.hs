{-# LANGUAGE DeriveGeneric, TemplateHaskell, BangPatterns, FlexibleInstances #-}

module AST where
import GHC.Generics

data VisibilityRepr = Public | Private deriving (Eq, Show, Generic)
data AccessRepr = Static deriving (Eq, Show, Generic)

data Id = Id String deriving (Eq, Show, Generic)
data Unop = UnopMinus | UnopNeg deriving (Eq, Show, Generic)
data Binop = Gt | Lt | DEq | Le | Ge | Ineq | Conj | Disj | Plus | Minus | Times | Div deriving (Eq, Show, Generic)
data LNum = LNum Int deriving (Eq, Show, Generic)

data Newable = NId Id | NIntA Expr | NAnyA Id Expr deriving (Eq, Show, Generic)

data Expr =
    RefExpr Reference
    | IxExpr IxReference
    | CallExpr Reference (Maybe ArgumentList)
    | UnopExpr Unop Expr
    | BinopExpr Expr Binop Expr
    | Parens Expr
    | IntLiteral LNum
    | BLiteral Bool
    | NewExpr Newable 
    | Commented Comment Expr deriving (Eq, Show, Generic)

newtype SafeString = SafeString { unwrapSafeString :: String } deriving Eq

instance Show SafeString where
    show = unwrapSafeString

data Comment = Empty1 | Empty2 | Empty3 | Empty4 | Empty5 | Empty6 | Empty7 | Empty8 | Empty9 | OneLineComment SafeString | MultilineComment SafeString deriving (Eq, Show, Generic)

data Program = Program [ClassDeclaration] deriving (Eq, Show, Generic)
data ClassDeclaration = ClassDeclaration Id [Either FieldDeclaration MethodDeclaration] deriving (Eq, Show, Generic)
data FieldDeclaration = FieldDeclaration Visibility Access Type Id deriving (Eq, Show, Generic)
data MethodDeclaration = MethodDeclaration Visibility Access (Maybe Type) Id (Maybe ParameterList) [Stmt] deriving (Eq, Show, Generic)
data Visibility = Visibility (Maybe VisibilityRepr) deriving (Eq, Show, Generic)
data Access = Access (Maybe AccessRepr) deriving (Eq, Show, Generic)
data Type = TInt | TBool | TId Id | TIntArray | TIdArray Id deriving (Eq, Show, Generic)
data ParameterList = ParameterList Type Id [(Type, Id)] deriving (Eq, Show, Generic)
data ArgumentList = ArgumentList Expr [Expr] deriving (Eq, Show, Generic)
data Reference = RId Id | RThis | RNested Reference Id deriving (Eq, Show, Generic)
data IxReference = IxReference Reference Expr deriving (Eq, Show, Generic)

data StmtList = StmtList [Stmt] deriving (Eq, Show, Generic)
data Stmt =
    Block StmtList
    | VarDecl Type Id Expr
    | VarAssn (Either Reference IxReference) Expr
    | MethCall Reference (Maybe ArgumentList)
    | Ret (Maybe Expr)
    | IfElse Expr Stmt (Maybe Stmt)
    | While Expr Stmt deriving (Eq, Show, Generic)