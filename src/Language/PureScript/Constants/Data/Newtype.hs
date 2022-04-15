module Language.PureScript.Constants.Data.Newtype where

import Language.PureScript.Names

pattern Newtype :: Qualified (ProperName 'ClassName)
pattern Newtype = Qualified (ByModule (ModuleName "Data.Newtype")) (ProperName "Newtype")
