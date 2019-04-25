module ArbitraryAST where
import AST

import GHC.Generics
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck
import Data.List

instance Arbitrary Id where
    arbitrary = do
        fc <- elements ['a'..'z']
        after <- listOf $ elements (['A'..'Z'] ++ ['a' .. 'z'] ++ ['_'] ++ ['0'..'9'])
        return $ Id (fc : after)

instance Arbitrary Unop where
    arbitrary = genericArbitrary

instance Arbitrary Binop where
    arbitrary = genericArbitrary

instance Arbitrary LNum where
    arbitrary = genericArbitrary

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeString :: Gen String
genSafeString = listOf genSafeChar

instance Arbitrary SafeString where
    arbitrary = SafeString <$> genSafeString

instance Arbitrary StmtList where
    arbitrary = do
        k <- choose (0, 3)
        x <- vectorOf k arbitrary
        return (StmtList x)

instance Arbitrary Program where
    arbitrary = do
        k <- choose (1, 3)
        x <- vectorOf k arbitrary
        return (Program x)

instance Arbitrary Comment where
    arbitrary = genericArbitrary

instance Arbitrary ClassDeclaration where
    arbitrary = genericArbitrary

instance Arbitrary FieldDeclaration where
    arbitrary = genericArbitrary

instance Arbitrary MethodDeclaration where
    arbitrary = genericArbitrary

instance Arbitrary Visibility where
    arbitrary = genericArbitrary

instance Arbitrary Access where
    arbitrary = genericArbitrary
instance Arbitrary Type where
    arbitrary = genericArbitrary

instance Arbitrary ParameterList where
    arbitrary = genericArbitrary

instance Arbitrary ArgumentList where
    arbitrary = do
        k <- choose (0, 3)
        e <- arbitrary :: Gen Expr
        l <- (vectorOf k (arbitrary :: Gen Expr))
        return $ ArgumentList e l

instance Arbitrary Reference where
    arbitrary = genericArbitrary

instance Arbitrary IxReference where
    arbitrary = genericArbitrary

instance Arbitrary Stmt where
    arbitrary = genericArbitrary

instance Arbitrary VisibilityRepr where
    arbitrary = genericArbitrary

instance Arbitrary AccessRepr where
    arbitrary = genericArbitrary

instance Arbitrary Expr where
    arbitrary = genericArbitrary

instance Arbitrary Newable where
    arbitrary = genericArbitrary