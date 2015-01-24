{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Fundamental built-in style. Defines no additional extensions or
-- configurations beyond the default printer.

module HIndent.Styles.Exact where

import           Data.Default
import           HIndent.Pretty
import           HIndent.Types

import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import qualified Language.Haskell.Exts.Annotated.ExactPrint as Exact
import           Language.Haskell.Exts.Annotated.Syntax

-- | Empty state.
data State = State

-- | The printer style.
exact :: Style
exact =
  Style {styleName = "exact"
        ,styleAuthor = ""
        ,styleDescription = "This style attempts to output the exact input."
        ,styleInitialState = State
        ,styleExtenders =
          [Extender module'
          ,Extender exactContext        
          ,Extender exactPat            
          ,Extender exactType           
          ,Extender exactExp            
          ,Extender exactStmt           
          ,Extender exactQualStmt       
          ,Extender exactDecl           
          ,Extender exactDeriving       
          ,Extender exactAlt            
          ,Extender exactAsst           
          ,Extender exactBangType       
          ,Extender exactBinds          
          ,Extender exactClassDecl      
          ,Extender exactConDecl        
          ,Extender exactFieldDecl      
          ,Extender exactFieldUpdate    
          ,Extender exactGuardedRhs     
          ,Extender exactInstDecl       
          ,Extender exactMatch          
          ,Extender exactPatField       
          ,Extender exactQualConDecl    
          ,Extender exactRhs            
          ,Extender exactSplice         
          ,Extender exactInstRule       
          ,Extender exactInstHead       
          ,Extender exactDeclHead       
          ,Extender exactSpecialCon     
          ,Extender exactOverlap        
          ,Extender exactSign           
          ,Extender exactModule         
          ,Extender exactBracket        
          ,Extender exactIPBind         
          ,Extender exactDataOrNew      
          ,Extender exactFunDep         
          ,Extender exactKind           
          ,Extender exactLiteral        
          ,Extender exactName           
          ,Extender exactQName          
          ,Extender exactQOp            
          ,Extender exactTyVarBind      
          ,Extender exactModuleHead     
          ,Extender exactModulePragma   
          ,Extender exactImportDecl     
          ,Extender exactModuleName     
          ,Extender exactImportSpecList 
          ,Extender exactImportSpec     
          ,Extender exactWarningText    
          ,Extender exactExportSpecList 
          ,Extender exactExportSpec     
          ]
        ,styleDefConfig = def}


type Extend f = forall t. t -> f NodeInfo -> Printer ()

exact' :: (Exact.ExactP ast, Pretty ast) => Extend ast
exact' _ a = write . T.fromText . T.pack $
             Exact.exactPrint (fmap nodeInfoSpan a) (fmap comInfoComment $ nodeInfoComments $ ann a)

module' :: Extend Module
module' = exact'

exactContext        :: Extend Context        
exactContext        = exact'
exactPat            :: Extend Pat            
exactPat            = exact'
exactType           :: Extend Type           
exactType           = exact'
exactExp            :: Extend Exp            
exactExp            = exact'
exactStmt           :: Extend Stmt           
exactStmt           = exact'
exactQualStmt       :: Extend QualStmt       
exactQualStmt       = exact'
exactDecl           :: Extend Decl           
exactDecl           = exact'
exactDeriving       :: Extend Deriving       
exactDeriving       = exact'
exactAlt            :: Extend Alt            
exactAlt            = exact'
exactAsst           :: Extend Asst           
exactAsst           = exact'
exactBangType       :: Extend BangType       
exactBangType       = exact'
exactBinds          :: Extend Binds          
exactBinds          = exact'
exactClassDecl      :: Extend ClassDecl      
exactClassDecl      = exact'
exactConDecl        :: Extend ConDecl        
exactConDecl        = exact'
exactFieldDecl      :: Extend FieldDecl      
exactFieldDecl      = exact'
exactFieldUpdate    :: Extend FieldUpdate    
exactFieldUpdate    = exact'
exactGuardedRhs     :: Extend GuardedRhs     
exactGuardedRhs     = exact'
exactInstDecl       :: Extend InstDecl       
exactInstDecl       = exact'
exactMatch          :: Extend Match          
exactMatch          = exact'
exactPatField       :: Extend PatField       
exactPatField       = exact'
exactQualConDecl    :: Extend QualConDecl    
exactQualConDecl    = exact'
exactRhs            :: Extend Rhs            
exactRhs            = exact'
exactSplice         :: Extend Splice         
exactSplice         = exact'
exactInstRule       :: Extend InstRule       
exactInstRule       = exact'
exactInstHead       :: Extend InstHead       
exactInstHead       = exact'
exactDeclHead       :: Extend DeclHead       
exactDeclHead       = exact'
exactSpecialCon     :: Extend SpecialCon     
exactSpecialCon     = exact'
exactOverlap        :: Extend Overlap        
exactOverlap        = exact'
exactSign           :: Extend Sign           
exactSign           = exact'
exactModule         :: Extend Module         
exactModule         = exact'
exactBracket        :: Extend Bracket        
exactBracket        = exact'
exactIPBind         :: Extend IPBind         
exactIPBind         = exact'
exactDataOrNew      :: Extend DataOrNew      
exactDataOrNew      = exact'
exactFunDep         :: Extend FunDep         
exactFunDep         = exact'
exactKind           :: Extend Kind           
exactKind           = exact'
exactLiteral        :: Extend Literal        
exactLiteral        = exact'
exactName           :: Extend Name           
exactName           = exact'
exactQName          :: Extend QName          
exactQName          = exact'
exactQOp            :: Extend QOp            
exactQOp            = exact'
exactTyVarBind      :: Extend TyVarBind      
exactTyVarBind      = exact'
exactModuleHead     :: Extend ModuleHead     
exactModuleHead     = exact'
exactModulePragma   :: Extend ModulePragma   
exactModulePragma   = exact'
exactImportDecl     :: Extend ImportDecl     
exactImportDecl     = exact'
exactModuleName     :: Extend ModuleName     
exactModuleName     = exact'
exactImportSpecList :: Extend ImportSpecList 
exactImportSpecList = exact'
exactImportSpec     :: Extend ImportSpec     
exactImportSpec     = exact'
exactWarningText    :: Extend WarningText    
exactWarningText    = exact'
exactExportSpecList :: Extend ExportSpecList 
exactExportSpecList = exact'
exactExportSpec     :: Extend ExportSpec     
exactExportSpec     = exact'
