module Utils.FormatIdent where

import {-# SOURCE #-} qualified Delta.Structures.Syntax as Stx
import Data.Text (Text)

formatIdent        :: Stx.Ident        -> Text
formatVarIdent     :: Stx.VarIdent     -> Text
formatTypeIdent    :: Stx.TypeIdent    -> Text
formatTypeVarIdent :: Stx.TypeVarIdent -> Text
formatModuleIdent  :: Stx.ModuleIdent  -> Text
