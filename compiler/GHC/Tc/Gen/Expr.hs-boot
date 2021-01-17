module GHC.Tc.Gen.Expr where
import GHC.Hs              ( HsExpr, LHsExpr, SyntaxExprRn
                           , SyntaxExprTc )
import GHC.Tc.Utils.TcType ( TcRhoType, TcSigmaType, SyntaxOpType, ExpType, ExpRhoType )
import GHC.Tc.Types        ( TcM )
import GHC.Tc.Types.Origin ( CtOrigin )
import GHC.Core.Type ( Mult )
import GHC.Hs.Extension.GhcPass ( GhcRn, GhcTc )

tcCheckPolyExpr, tcCheckPolyExprNC ::
          LHsExpr GhcRn
       -> TcSigmaType
       -> TcM (LHsExpr GhcTc)

tcMonoExpr, tcMonoExprNC ::
          LHsExpr GhcRn
       -> ExpRhoType
       -> TcM (LHsExpr GhcTc)
tcCheckMonoExpr, tcCheckMonoExprNC ::
          LHsExpr GhcRn
       -> TcRhoType
       -> TcM (LHsExpr GhcTc)

tcExpr :: HsExpr GhcRn -> ExpRhoType -> TcM (HsExpr GhcTc)

tcInferRho, tcInferRhoNC ::
          LHsExpr GhcRn -> TcM (LHsExpr GhcTc, TcRhoType)

tcSyntaxOp :: CtOrigin
           -> SyntaxExprRn
           -> [SyntaxOpType]           -- ^ shape of syntax operator arguments
           -> ExpType                  -- ^ overall result type
           -> ([TcSigmaType] -> [Mult] -> TcM a) -- ^ Type check any arguments
           -> TcM (a, SyntaxExprTc)

tcSyntaxOpGen :: CtOrigin
              -> SyntaxExprRn
              -> [SyntaxOpType]
              -> SyntaxOpType
              -> ([TcSigmaType] -> [Mult] -> TcM a)
              -> TcM (a, SyntaxExprTc)

