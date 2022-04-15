-- | Various constants which refer to things in Prim
module Language.PureScript.Constants.Prim where

import Data.String (IsString)
import Language.PureScript.Names

-- Prim values

undefined :: forall a. (IsString a) => a
undefined = "undefined"

-- Prim

pattern Prim :: ModuleName
pattern Prim = ModuleName "Prim"

pattern Partial :: Qualified (ProperName 'ClassName)
pattern Partial = Qualified (ByModule Prim) (ProperName "Partial")

pattern Record :: Qualified (ProperName 'TypeName)
pattern Record = Qualified (ByModule Prim) (ProperName "Record")

pattern Type :: Qualified (ProperName 'TypeName)
pattern Type = Qualified (ByModule Prim) (ProperName "Type")

pattern Constraint :: Qualified (ProperName 'TypeName)
pattern Constraint = Qualified (ByModule Prim) (ProperName "Constraint")

pattern Function :: Qualified (ProperName 'TypeName)
pattern Function = Qualified (ByModule Prim) (ProperName "Function")

pattern Array :: Qualified (ProperName 'TypeName)
pattern Array = Qualified (ByModule Prim) (ProperName "Array")

pattern Row :: Qualified (ProperName 'TypeName)
pattern Row = Qualified (ByModule Prim) (ProperName "Row")

-- Prim.Boolean

pattern PrimBoolean :: ModuleName
pattern PrimBoolean = ModuleName "Prim.Boolean"

booleanTrue :: Qualified (ProperName 'TypeName)
booleanTrue = Qualified (ByModule PrimBoolean) (ProperName "True")

booleanFalse :: Qualified (ProperName 'TypeName)
booleanFalse = Qualified (ByModule PrimBoolean) (ProperName "False")

-- Prim.Coerce

pattern PrimCoerce :: ModuleName
pattern PrimCoerce = ModuleName "Prim.Coerce"

pattern Coercible :: Qualified (ProperName 'ClassName)
pattern Coercible = Qualified (ByModule PrimCoerce) (ProperName "Coercible")

-- Prim.Ordering

pattern PrimOrdering :: ModuleName
pattern PrimOrdering = ModuleName "Prim.Ordering"

orderingLT :: Qualified (ProperName 'TypeName)
orderingLT = Qualified (ByModule PrimOrdering) (ProperName "LT")

orderingEQ :: Qualified (ProperName 'TypeName)
orderingEQ = Qualified (ByModule PrimOrdering) (ProperName "EQ")

orderingGT :: Qualified (ProperName 'TypeName)
orderingGT = Qualified (ByModule PrimOrdering) (ProperName "GT")

-- Prim.Row

pattern PrimRow :: ModuleName
pattern PrimRow = ModuleName "Prim.Row"

pattern RowUnion :: Qualified (ProperName 'ClassName)
pattern RowUnion = Qualified (ByModule PrimRow) (ProperName "Union")

pattern RowNub :: Qualified (ProperName 'ClassName)
pattern RowNub = Qualified (ByModule PrimRow) (ProperName "Nub")

pattern RowCons :: Qualified (ProperName 'ClassName)
pattern RowCons = Qualified (ByModule PrimRow) (ProperName "Cons")

pattern RowLacks :: Qualified (ProperName 'ClassName)
pattern RowLacks = Qualified (ByModule PrimRow) (ProperName "Lacks")

-- Prim.RowList

pattern PrimRowList :: ModuleName
pattern PrimRowList = ModuleName "Prim.RowList"

pattern RowToList :: Qualified (ProperName 'ClassName)
pattern RowToList = Qualified (ByModule PrimRowList) (ProperName "RowToList")

pattern RowListNil :: Qualified (ProperName 'TypeName)
pattern RowListNil = Qualified (ByModule PrimRowList) (ProperName "Nil")

pattern RowListCons :: Qualified (ProperName 'TypeName)
pattern RowListCons = Qualified (ByModule PrimRowList) (ProperName "Cons")

-- Prim.Int

pattern PrimInt :: ModuleName
pattern PrimInt = ModuleName "Prim.Int"

pattern IntAdd :: Qualified (ProperName 'ClassName)
pattern IntAdd = Qualified (ByModule PrimInt) (ProperName "Add")

pattern IntCompare :: Qualified (ProperName 'ClassName)
pattern IntCompare = Qualified (ByModule PrimInt) (ProperName "Compare")

pattern IntMul :: Qualified (ProperName 'ClassName)
pattern IntMul = Qualified (ByModule PrimInt) (ProperName "Mul")

-- Prim.Symbol

pattern PrimSymbol :: ModuleName
pattern PrimSymbol = ModuleName "Prim.Symbol"

pattern SymbolCompare :: Qualified (ProperName 'ClassName)
pattern SymbolCompare = Qualified (ByModule PrimSymbol) (ProperName "Compare")

pattern SymbolAppend :: Qualified (ProperName 'ClassName)
pattern SymbolAppend = Qualified (ByModule PrimSymbol) (ProperName "Append")

pattern SymbolCons :: Qualified (ProperName 'ClassName)
pattern SymbolCons = Qualified (ByModule PrimSymbol) (ProperName "Cons")

-- Prim.TypeError

pattern PrimTypeError :: ModuleName
pattern PrimTypeError = ModuleName "Prim.TypeError"

pattern Fail :: Qualified (ProperName 'ClassName)
pattern Fail = Qualified (ByModule PrimTypeError) (ProperName "Fail")

pattern Warn :: Qualified (ProperName 'ClassName)
pattern Warn = Qualified (ByModule PrimTypeError) (ProperName "Warn")

primModules :: [ModuleName]
primModules = [Prim, PrimBoolean, PrimCoerce, PrimOrdering, PrimRow, PrimRowList, PrimSymbol, PrimInt, PrimTypeError]

typ :: forall a. (IsString a) => a
typ = "Type"

kindOrdering :: forall a. (IsString a) => a
kindOrdering = "Ordering"

kindRowList :: forall a. (IsString a) => a
kindRowList = "RowList"

symbol :: forall a. (IsString a) => a
symbol = "Symbol"

doc :: forall a. (IsString a) => a
doc = "Doc"

row :: forall a. (IsString a) => a
row = "Row"

constraint :: forall a. (IsString a) => a
constraint = "Constraint"

-- Modules

prim :: forall a. (IsString a) => a
prim = "Prim"

moduleBoolean :: forall a. (IsString a) => a
moduleBoolean = "Boolean"

moduleCoerce :: forall a. (IsString a) => a
moduleCoerce = "Coerce"

moduleOrdering :: forall a. (IsString a) => a
moduleOrdering = "Ordering"

moduleRow :: forall a. (IsString a) => a
moduleRow = "Row"

moduleRowList :: forall a. (IsString a) => a
moduleRowList = "RowList"

moduleSymbol :: forall a. (IsString a) => a
moduleSymbol = "Symbol"

moduleInt :: forall a. (IsString a) => a
moduleInt = "Int"

typeError :: forall a. (IsString a) => a
typeError = "TypeError"
