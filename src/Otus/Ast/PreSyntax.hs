module Otus.Ast.PreSyntax (
  PreSyntax (..),
) where

import Otus.Ast.Id

data PreSyntax
  = SVar Name
  | SLetIn Name (Maybe PreSyntax) PreSyntax
  | SLam Name (Maybe PreSyntax) PreSyntax
  | SPi Name PreSyntax PreSyntax
  | SApp PreSyntax PreSyntax
  | SId (Maybe PreSyntax) PreSyntax PreSyntax
  | SRefl (Maybe PreSyntax)
  | SJ PreSyntax PreSyntax PreSyntax
  | SType Int
  | SHole
  deriving (Eq, Show)
