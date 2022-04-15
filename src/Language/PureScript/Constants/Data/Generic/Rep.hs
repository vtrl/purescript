module Language.PureScript.Constants.Data.Generic.Rep where

import Language.PureScript.Names

pattern DataGenericRep :: ModuleName
pattern DataGenericRep = ModuleName "Data.Generic.Rep"

pattern Generic :: Qualified (ProperName 'ClassName)
pattern Generic = Qualified (ByModule DataGenericRep) (ProperName "Generic")

to :: Qualified Ident
to = Qualified (ByModule DataGenericRep) (Ident "to")

from :: Qualified Ident
from = Qualified (ByModule DataGenericRep) (Ident "from")

pattern NoConstructors :: Qualified (ProperName a)
pattern NoConstructors = Qualified (ByModule DataGenericRep) (ProperName "NoConstructors")

pattern NoArguments :: Qualified (ProperName a)
pattern NoArguments = Qualified (ByModule DataGenericRep) (ProperName "NoArguments")

pattern Sum :: Qualified (ProperName a)
pattern Sum = Qualified (ByModule DataGenericRep) (ProperName "Sum")

pattern Inl :: Qualified (ProperName a)
pattern Inl = Qualified (ByModule DataGenericRep) (ProperName "Inl")

pattern Inr :: Qualified (ProperName a)
pattern Inr = Qualified (ByModule DataGenericRep) (ProperName "Inr")

pattern Product :: Qualified (ProperName a)
pattern Product = Qualified (ByModule DataGenericRep) (ProperName "Product")

pattern Constructor :: Qualified (ProperName a)
pattern Constructor = Qualified (ByModule DataGenericRep) (ProperName "Constructor")

pattern Argument :: Qualified (ProperName a)
pattern Argument = Qualified (ByModule DataGenericRep) (ProperName "Argument")
