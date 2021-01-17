{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module GHC.Parser.Types
   ( SumOrTuple(..)
   , pprSumOrTuple
   , PatBuilder(..)
   , DataConBuilder(..)
   )
where

import GHC.Prelude
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Types.Name.Reader
import GHC.Hs.Extension.GhcPass
import GHC.Hs.Lit
import GHC.Hs.Pat
import GHC.Hs.Type
import GHC.Utils.Outputable as Outputable
import GHC.Data.OrdList

import Data.Foldable

data SumOrTuple b
  = Sum ConTag Arity (Located b)
  | Tuple [Located (Maybe (Located b))]

pprSumOrTuple :: Outputable b => Boxity -> SumOrTuple b -> SDoc
pprSumOrTuple boxity = \case
    Sum alt arity e ->
      parOpen <+> ppr_bars (alt - 1) <+> ppr e <+> ppr_bars (arity - alt)
              <+> parClose
    Tuple xs ->
      parOpen <> (fcat . punctuate comma $ map (maybe empty ppr . unLoc) xs)
              <> parClose
  where
    ppr_bars n = hsep (replicate n (Outputable.char '|'))
    (parOpen, parClose) =
      case boxity of
        Boxed -> (text "(", text ")")
        Unboxed -> (text "(#", text "#)")


-- | See Note [Ambiguous syntactic categories] and Note [PatBuilder]
data PatBuilder p
  = PatBuilderPat (Pat p)
  | PatBuilderPar (Located (PatBuilder p))
  | PatBuilderApp (Located (PatBuilder p)) (Located (PatBuilder p))
  | PatBuilderAppType (Located (PatBuilder p)) (HsPatSigType GhcPs)
  | PatBuilderOpApp (Located (PatBuilder p)) (Located RdrName) (Located (PatBuilder p))
  | PatBuilderVar (Located RdrName)
  | PatBuilderOverLit (HsOverLit GhcPs)

instance Outputable (PatBuilder GhcPs) where
  ppr (PatBuilderPat p) = ppr p
  ppr (PatBuilderPar (L _ p)) = parens (ppr p)
  ppr (PatBuilderApp (L _ p1) (L _ p2)) = ppr p1 <+> ppr p2
  ppr (PatBuilderAppType (L _ p) t) = ppr p <+> text "@" <> ppr t
  ppr (PatBuilderOpApp (L _ p1) op (L _ p2)) = ppr p1 <+> ppr op <+> ppr p2
  ppr (PatBuilderVar v) = ppr v
  ppr (PatBuilderOverLit l) = ppr l

-- | An accumulator to build a prefix data constructor,
--   e.g. when parsing @MkT A B C@, the accumulator will evolve as follows:
--
--  @
--  1. PrefixDataConBuilder []        MkT
--  2. PrefixDataConBuilder [A]       MkT
--  3. PrefixDataConBuilder [A, B]    MkT
--  4. PrefixDataConBuilder [A, B, C] MkT
--  @
--
--  There are two reasons we have a separate builder type instead of using
--  @HsConDeclDetails GhcPs@ directly:
--
--  1. It's faster, because 'OrdList' gives us constant-time snoc.
--  2. Having a separate type helps ensure that we don't forget to finalize a
--     'RecTy' into a 'RecCon' (we do that in 'dataConBuilderDetails').
--
--  See Note [PatBuilder] for another builder type used in the parser.
--  Here the technique is similar, but the motivation is different.
data DataConBuilder
  = PrefixDataConBuilder
      (OrdList (LHsType GhcPs))  -- Data constructor fields
      (Located RdrName)          -- Data constructor name
  | InfixDataConBuilder
      (LHsType GhcPs)   -- LHS field
      (Located RdrName) -- Data constructor name
      (LHsType GhcPs)   -- RHS field

instance Outputable DataConBuilder where
  ppr (PrefixDataConBuilder flds data_con) =
    hang (ppr data_con) 2 (sep (map ppr (toList flds)))
  ppr (InfixDataConBuilder lhs data_con rhs) =
    ppr lhs <+> ppr data_con <+> ppr rhs

