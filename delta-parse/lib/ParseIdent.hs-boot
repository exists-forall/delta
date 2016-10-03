module ParseIdent where

import ParseUtils
import {-# SOURCE #-} qualified Syntax as Stx

data CheckReserved = AllowReserved | ForbidReserved

ident'        :: CheckReserved -> Parser Stx.Ident
moduleIdent'  :: CheckReserved -> Parser Stx.ModuleIdent
varIdent'     :: CheckReserved -> Parser Stx.VarIdent
typeIdent'    :: CheckReserved -> Parser Stx.TypeIdent
typeVarIdent' :: CheckReserved -> Parser Stx.TypeVarIdent
