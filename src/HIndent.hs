{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Haskell indenter.

module HIndent where

import           Control.Monad.State
import           Data.Int
import           Data.List (intersperse)
import           Data.Monoid
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO as TIO
import           Language.Haskell.Exts.Parser
import qualified Language.Haskell.Exts.Pretty as P
import           Language.Haskell.Exts.Syntax

format :: String -> IO ()
format x =
  case parseExp x of
    ParseOk v ->
      TIO.putStrLn
        (T.toLazyText
           (psOutput
              (execState (runPrinter (pretty v))
                         (PrintState 0 mempty False 0))))
    ParseFailed{} ->
      putStrLn x

test m =
  (execState (runPrinter m)
             (PrintState 0 mempty False 0))

newtype Printer a = Printer { runPrinter :: State PrintState a }
  deriving (Monad,Functor,MonadState PrintState)

-- | The state of the pretty printer.
data PrintState = PrintState
  { psIndentLevel :: !Int64   -- ^ Current indentation level.
  , psOutput      :: !Builder -- ^ The current output.
  , psNewline     :: !Bool    -- ^ Just outputted a newline?)
  , psColumn      :: !Int64   -- ^ Current column.
  } deriving (Show)

-- | Print the given printer indented.
indented :: Int64 -> Printer a -> Printer a
indented i p =
  do level <- gets psIndentLevel
     modify (\s -> s { psIndentLevel = level + i })
     m <- p
     modify (\s -> s { psIndentLevel = level })
     return m

-- | Print the given printer indented.
column :: Int64 -> Printer a -> Printer a
column i p =
  do level <- gets psIndentLevel
     modify (\s -> s { psIndentLevel = i })
     m <- p
     modify (\s -> s { psIndentLevel = level })
     return m

-- | Output a newline.
newline :: Printer ()
newline = do
  write "\n"
  modify (\s -> s { psNewline = True })

-- | Make the latter's indentation depend upon the end column of the
-- former.
depend :: Printer () -> Printer b -> Printer b
depend maker dependent =
  do maker
     col <- gets psColumn
     column col dependent

-- | Write out a string, updating the current position information.
write :: Builder -> Printer ()
write x = do
  state <- get
  let out =
        if psNewline state
           then T.fromText (T.replicate (fromIntegral (psIndentLevel state)) " ") <> x
           else x
      out' = T.toLazyText out
  modify
    (\s ->
       s { psOutput  = psOutput state <> out
         , psNewline = False
         , psColumn  =
             if additionalLines > 0
                then LT.length (LT.concat (take 1 (reverse srclines)))
                else psColumn state + LT.length out'
         })
  where
        x' = T.toLazyText x
        srclines = LT.lines x'
        additionalLines = LT.length (LT.filter (=='\n') x')

class Pretty a where
  pretty :: a -> Printer ()

pretty' :: P.Pretty a => a -> Printer ()
pretty' = write . T.fromText . T.pack . P.prettyPrint

instance Pretty Alt where
  pretty x =
    case x of
      Alt _ pat galts binds ->
        undefined

instance Pretty Asst where
  pretty x =
    case x of
      ClassA _ _ -> undefined
      InfixA _ _ _ -> undefined
      IParam _ _ -> undefined
      EqualP _ _ -> undefined

instance Pretty BangType where
  pretty x =
    case x of
      BangedTy _ -> undefined
      UnBangedTy _ -> undefined
      UnpackedTy _ -> undefined

instance Pretty Binds where
  pretty x =
    case x of
      BDecls _ -> undefined
      IPBinds _ -> undefined

instance Pretty Bracket where
  pretty x =
    case x of
      ExpBracket _ -> undefined
      PatBracket _ -> undefined
      TypeBracket _ -> undefined
      DeclBracket _ -> undefined

instance Pretty ClassDecl where
  pretty x =
    undefined

instance Pretty ConDecl where
  pretty x =
    case x of
      ConDecl _ _ -> undefined
      InfixConDecl _ _ _ -> undefined
      RecDecl _ _ -> undefined

instance Pretty Decl where
  pretty x =
    case x of
      TypeDecl _ _ _ _ -> undefined
      TypeFamDecl _ _ _ _ -> undefined
      DataDecl _ _ _ _ _ _ _ -> undefined
      GDataDecl _ _ _ _ _ _ _ _ -> undefined
      DataFamDecl _ _ _ _ _ -> undefined
      TypeInsDecl _ _ _ -> undefined
      DataInsDecl _ _ _ _ _ -> undefined
      GDataInsDecl _ _ _ _ _ _ -> undefined
      ClassDecl _ _ _ _ _ _ -> undefined
      InstDecl _ _ _ _ _ -> undefined
      DerivDecl _ _ _ _ -> undefined
      InfixDecl _ _ _ _ -> undefined
      DefaultDecl _ _ -> undefined
      SpliceDecl _ _ -> undefined
      TypeSig _ _ _ -> undefined
      FunBind _ -> undefined
      PatBind _ p _ _ _ -> undefined
      ForImp _ _ _ _ _ _ -> undefined
      ForExp _ _ _ _ _ -> undefined
      RulePragmaDecl _ _ -> undefined
      DeprPragmaDecl _ _ -> undefined
      WarnPragmaDecl _ _ -> undefined
      InlineSig _ _ _ _ -> undefined
      InlineConlikeSig _ _ _ -> undefined
      SpecSig _ _ _ _ -> undefined
      SpecInlineSig _ _ _ _ _ -> undefined
      InstSig _ _ _ _ -> undefined
      AnnPragma _ _ -> undefined

instance Pretty Exp where
  pretty x =
    case x of
      InfixApp a op b ->
        do pretty a
           write " "
           pretty op
           newline
           pretty b
      App f a ->
        case flatten f [a] of
          (f,args) ->
            depend (do pretty f
                       write " ")
                   (sequence_
                      (intersperse newline
                                   (map pretty args)))
      NegApp e ->
        depend (write "-")
               (pretty e)
      Lambda _ ps e ->
        depend (write "\\")
               (do sequence_ (intersperse (write " ")
                                          (map pretty ps))
                   write " -> "
                   newline
                   indented 1 (pretty e))
      Let _ e ->
        do write "let"
           newline
           depend (write "in ")
                  (pretty e)
      If p t e ->
        do depend (write "if ")
                  (do pretty p
                      newline
                      depend (write "then ")
                             (pretty t)
                      newline
                      depend (write "else ")
                             (pretty t))
      Paren e ->
        depend (write "(")
               (do pretty e
                   write ")")
      MultiIf _ -> undefined
      Case e alts ->
        do write "case "
           indented 5 (pretty e)
           write " of"
           newline
           indented 2 (mapM_ pretty alts)
      Do _ -> undefined
      MDo _ -> undefined
      Tuple _ _ -> undefined
      TupleSection _ _ -> undefined
      List _ -> undefined
      LeftSection _ _ -> undefined
      RightSection _ _ -> undefined
      RecConstr _ _ -> undefined
      RecUpdate _ _ -> undefined
      EnumFrom _ -> undefined
      EnumFromTo _ _ -> undefined
      EnumFromThen _ _ -> undefined
      EnumFromThenTo _ _ _ -> undefined
      ListComp _ _ -> undefined
      ParComp _ _ -> undefined
      ExpTypeSig _ _ _ -> undefined
      VarQuote _ -> undefined
      TypQuote _ -> undefined
      BracketExp _ -> undefined
      SpliceExp _ -> undefined
      QuasiQuote _ _ -> undefined
      CorePragma _ _ -> undefined
      SCCPragma _ _ -> undefined
      GenPragma _ _ _ _ -> undefined
      Proc _ _ _ -> undefined
      LeftArrApp _ _ -> undefined
      RightArrApp _ _ -> undefined
      LeftArrHighApp _ _ -> undefined
      RightArrHighApp _ _ -> undefined
      LCase _ -> undefined
      XTag{} -> pretty' x
      XETag{} -> pretty' x
      XPcdata{} -> pretty' x
      XExpTag{} -> pretty' x
      XChildTag{} -> pretty' x
      Var{} -> pretty' x
      IPVar{} -> pretty' x
      Con{} -> pretty' x
      Lit{} -> pretty' x

flatten :: Exp -> [Exp] -> (Exp,[Exp])
flatten (App f a) b = flatten f (a : b)
flatten f as = (f,as)

instance Pretty FieldUpdate where
  pretty x =
    case x of
      FieldUpdate _ _ -> undefined
      FieldPun _ -> undefined
      FieldWildcard -> undefined

instance Pretty GadtDecl where
  pretty x =
    case x of
      GadtDecl _ _ _ -> undefined

instance Pretty GuardedAlt where
  pretty x =
    case x of
      GuardedAlt _ _ _ -> undefined

instance Pretty GuardedAlts where
  pretty x =
    case x of
      UnGuardedAlt _ -> undefined
      GuardedAlts _ -> undefined

instance Pretty GuardedRhs where
  pretty x =
    case x of
      GuardedRhs _ _ _ -> undefined

instance Pretty IPBind where
  pretty x =
    case x of
      IPBind _ _ _ -> undefined

instance Pretty IfAlt where
  pretty x =
    case x of
      IfAlt _ _ -> undefined

instance Pretty InstDecl where
  pretty x =
    undefined

instance Pretty Match where
  pretty x =
    case x of
      Match _ _ _ _ _ _ -> undefined

instance Pretty Module where
  pretty x =
    case x of
      Module _ _ _ _ _ _ _ -> undefined

instance Pretty Pat where
  pretty x =
    case x of
      PLit _ -> undefined
      PNeg _ -> undefined
      PNPlusK _ _ -> undefined
      PInfixApp _ _ _ -> undefined
      PApp _ _ -> undefined
      PTuple _ _ -> undefined
      PList _ -> undefined
      PParen _ -> undefined
      PRec _ _ -> undefined
      PAsPat _ _ -> undefined
      PWildCard -> undefined
      PIrrPat _ -> undefined
      PatTypeSig _ _ _ -> undefined
      PViewPat _ _ -> undefined
      PQuasiQuote _ _ -> undefined
      PBangPat p -> undefined
      PRPat{} -> pretty' x
      PXTag{} -> pretty' x
      PXETag{} -> pretty' x
      PXPcdata{} -> pretty' x
      PXPatTag{} -> pretty' x
      PXRPats{} -> pretty' x
      PVar{} -> pretty' x

instance Pretty PatField where
  pretty x =
    case x of
      PFieldPat _ _ -> undefined
      PFieldPun _ -> undefined
      PFieldWildcard -> undefined

instance Pretty QualConDecl where
  pretty x =
    case x of
      QualConDecl _ _ _ _ -> undefined

instance Pretty QualStmt where
  pretty x =
    case x of
      QualStmt _ -> undefined
      ThenTrans _ -> undefined
      ThenBy _ _ -> undefined
      GroupBy _ -> undefined
      GroupUsing _ -> undefined
      GroupByUsing _ _ -> undefined

instance Pretty Rhs where
  pretty x =
    case x of
      UnGuardedRhs _ -> undefined
      GuardedRhss _ -> undefined

instance Pretty Rule where
  pretty =
    case undefined of
      Rule _ _ _ _ _ -> undefined

instance Pretty RuleVar where
  pretty x =
    case x of
      RuleVar _ -> undefined
      TypedRuleVar _ _ -> undefined

instance Pretty Splice where
  pretty x =
    case undefined of
      IdSplice _ -> undefined
      ParenSplice _ -> undefined

instance Pretty Stmt where
  pretty x =
    case x of
      Generator _ _ _ -> undefined
      Qualifier _ -> undefined
      LetStmt _ -> undefined
      RecStmt _ -> undefined

instance Pretty Type where
  pretty x =
    case x of
      TyForall _ _ _ -> undefined
      TyFun _ _ -> undefined
      TyTuple _ _ -> undefined
      TyList _ -> undefined
      TyApp _ _ -> undefined
      TyVar _ -> undefined
      TyCon _ -> undefined
      TyParen _ -> undefined
      TyInfix _ _ _ -> undefined
      TyKind _ _ -> undefined
      TyPromoted _ -> undefined

instance Pretty WarningText where
  pretty x =
    case x of
      DeprText _ -> undefined
      WarnText _ -> undefined

instance Pretty Tool where
  pretty x =
    case x of
      GHC -> write "GHC"
      HUGS -> write "HUGS"
      NHC98 -> write "NHC98"
      YHC -> write "YHC"
      HADDOCK -> write "HADDOCK"
      UnknownTool x -> write (T.fromText (T.pack x))

instance Pretty Activation where
  pretty = pretty'

instance Pretty Annotation where
  pretty = pretty'

instance Pretty Assoc where
  pretty = pretty'

instance Pretty CName where
  pretty = pretty'

instance Pretty CallConv where
  pretty = pretty'

instance Pretty DataOrNew where
  pretty = pretty'

instance Pretty ExportSpec where
  pretty = pretty'

instance Pretty FunDep where
  pretty = pretty'

instance Pretty IPName where
  pretty = pretty'

instance Pretty ImportSpec where
  pretty = pretty'

instance Pretty ImportDecl where
  pretty = pretty'

instance Pretty Kind where
  pretty = pretty'

instance Pretty Literal where
  pretty = pretty'

instance Pretty ModulePragma where
  pretty = pretty'

instance Pretty Name where
  pretty = pretty'

instance Pretty Op where
  pretty = pretty'

instance Pretty PXAttr where
  pretty = pretty'

instance Pretty Promoted where
  pretty = pretty'

instance Pretty QName where
  pretty = pretty'

instance Pretty QOp where
  pretty = pretty'

instance Pretty RPat where
  pretty = pretty'

instance Pretty RPatOp where
  pretty = pretty'

instance Pretty Safety where
  pretty = pretty'

instance Pretty SpecialCon where
  pretty = pretty'

instance Pretty TyVarBind where
  pretty = pretty'

instance Pretty XAttr where
  pretty = pretty'

instance Pretty XName where
  pretty = pretty'
