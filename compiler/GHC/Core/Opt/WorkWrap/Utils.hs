{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

A library for the ``worker\/wrapper'' back-end to the strictness analyser
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.Core.Opt.WorkWrap.Utils
   ( WwOpts(..), initWwOpts, mkWwBodies, mkWWstr, mkWorkerArgs
   , DataConPatContext(..), wantToUnboxArg
   , findTypeShape
   , isWorkerSmallEnough
   )
where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Core
import GHC.Core.Utils   ( exprType, mkCast, mkDefaultCase, mkSingleAltCase
                        , dataConRepFSInstPat, normSplitTyConApp_maybe )
import GHC.Types.Id
import GHC.Types.Id.Info ( JoinArity )
import GHC.Core.DataCon
import GHC.Types.Demand
import GHC.Types.Cpr
import GHC.Types.Unbox
import GHC.Core.Make    ( mkAbsentErrorApp, mkCoreUbxTup
                        , mkCoreApp, mkWildValBinder, mkCoreLet )
import GHC.Types.Id.Make ( voidArgId, voidPrimId )
import GHC.Builtin.Types      ( tupleDataCon, unboxedUnitTy )
import GHC.Types.Literal ( absentLiteralOf, rubbishLit )
import GHC.Types.Var.Env ( mkInScopeSet )
import GHC.Types.Var.Set ( VarSet )
import GHC.Core.Type
import GHC.Core.Multiplicity
import GHC.Core.Predicate ( isClassPred )
import GHC.Types.RepType  ( isVoidTy, typePrimRep )
import GHC.Core.Coercion
import GHC.Core.FamInstEnv
import GHC.Types.Basic       ( Boxity(..) )
import GHC.Core.TyCon
import GHC.Core.TyCon.RecWalk
import GHC.Types.Unique.Supply
import GHC.Types.Unique
import GHC.Types.Name ( getOccFS )
import GHC.Data.Maybe
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Data.FastString
import GHC.Data.List.SetOps
import GHC.Data.OrdList

import Control.Monad (zipWithM)
import Control.Applicative ((<|>))
import Data.List (unzip4)

{-
************************************************************************
*                                                                      *
\subsection[mkWrapperAndWorker]{@mkWrapperAndWorker@}
*                                                                      *
************************************************************************

Here's an example.  The original function is:

\begin{verbatim}
g :: forall a . Int -> [a] -> a

g = \/\ a -> \ x ys ->
        case x of
          0 -> head ys
          _ -> head (tail ys)
\end{verbatim}

From this, we want to produce:
\begin{verbatim}
-- wrapper (an unfolding)
g :: forall a . Int -> [a] -> a

g = \/\ a -> \ x ys ->
        case x of
          I# x# -> $wg a x# ys
            -- call the worker; don't forget the type args!

-- worker
$wg :: forall a . Int# -> [a] -> a

$wg = \/\ a -> \ x# ys ->
        let
            x = I# x#
        in
            case x of               -- note: body of g moved intact
              0 -> head ys
              _ -> head (tail ys)
\end{verbatim}

Something we have to be careful about:  Here's an example:

\begin{verbatim}
-- "f" strictness: U(P)U(P)
f (I# a) (I# b) = a +# b

g = f   -- "g" strictness same as "f"
\end{verbatim}

\tr{f} will get a worker all nice and friendly-like; that's good.
{\em But we don't want a worker for \tr{g}}, even though it has the
same strictness as \tr{f}.  Doing so could break laziness, at best.

Consequently, we insist that the number of strictness-info items is
exactly the same as the number of lambda-bound arguments.  (This is
probably slightly paranoid, but OK in practice.)  If it isn't the
same, we ``revise'' the strictness info, so that we won't propagate
the unusable strictness-info into the interfaces.


************************************************************************
*                                                                      *
\subsection{The worker wrapper core}
*                                                                      *
************************************************************************

@mkWwBodies@ is called when doing the worker\/wrapper split inside a module.
-}

data WwOpts
  = MkWwOpts
  { wo_fam_envs          :: !FamInstEnvs
  , wo_cpr_anal          :: !Bool
  , wo_fun_to_thunk      :: !Bool
  , wo_max_worker_args   :: !Int
  , wo_output_file       :: Maybe String
  }

initWwOpts :: DynFlags -> FamInstEnvs -> WwOpts
initWwOpts dflags fam_envs = MkWwOpts
  { wo_fam_envs          = fam_envs
  , wo_cpr_anal          = gopt Opt_CprAnal dflags
  , wo_fun_to_thunk      = gopt Opt_FunToThunk dflags
  , wo_max_worker_args   = maxWorkerArgs dflags
  , wo_output_file       = outputFile dflags
  }

type WwResult
  = ([Demand],              -- Demands for worker (value) args
     JoinArity,             -- Number of worker (type OR value) args
     Id -> CoreExpr,        -- Wrapper body, lacking only the worker Id
     CoreExpr -> CoreExpr)  -- Worker body, lacking the original function rhs

mkWwBodies :: WwOpts
           -> VarSet         -- ^ Free vars of RHS
                             -- See Note [Freshen WW arguments]
           -> Id             -- ^ The original function
           -> [Demand]       -- ^ Strictness of original function
                             -- (derived from 'idStrictness')
           -> Cpr            -- ^ Info about function result
           -> UniqSM (Maybe WwResult)

-- wrap_fn_args E       = \x y -> E
-- work_fn_args E       = E x y

-- wrap_fn_str E        = case x of { (a,b) ->
--                        case a of { (a1,a2) ->
--                        E a1 a2 b y }}
-- work_fn_str E        = \a1 a2 b y ->
--                        let a = (a1,a2) in
--                        let x = (a,b) in
--                        E

mkWwBodies opts rhs_fvs fun_id arg_dmds cpr_info
  = do  { let empty_subst = mkEmptyTCvSubst (mkInScopeSet rhs_fvs)
                -- See Note [Freshen WW arguments]

        ; (wrap_args, wrap_fn_args, work_fn_args, res_ty)
             <- mkWWargs empty_subst fun_ty arg_dmds
        ; (useful1, work_args, wrap_fn_str, work_fn_str)
             <- mkWWstr opts arg_ubx_strat wrap_args

        -- Do CPR w/w.  See Note [Always do CPR w/w]
        ; (useful2, wrap_fn_cpr, work_fn_cpr, cpr_res_ty)
              <- mkWWcpr_start opts ret_ubx_strat res_ty cpr_info

        ; let (work_lam_args, work_call_args) = mkWorkerArgs (wo_fun_to_thunk opts) work_args cpr_res_ty
              worker_args_dmds = [idDemandInfo v | v <- work_call_args, isId v]
              wrapper_body = wrap_fn_args . wrap_fn_cpr . wrap_fn_str . applyToVars work_call_args . Var
              worker_body = mkLams work_lam_args. work_fn_str . work_fn_cpr . work_fn_args

        ; if isWorkerSmallEnough (wo_max_worker_args opts) (length arg_dmds) work_args
             && not (too_many_args_for_join_point wrap_args)
             && ((useful1 && not only_one_void_argument) || useful2)
          then return (Just (worker_args_dmds, length work_call_args,
                       wrapper_body, worker_body))
          else return Nothing
        }
        -- We use an INLINE unconditionally, even if the wrapper turns out to be
        -- something trivial like
        --      fw = ...
        --      f = __inline__ (coerce T fw)
        -- The point is to propagate the coerce to f's call sites, so even though
        -- f's RHS is now trivial (size 1) we still want the __inline__ to prevent
        -- fw from being inlined into f's RHS
  where
    fun_ty        = idType fun_id
    mb_join_arity = isJoinId_maybe fun_id

    arg_ubx_strat :: UnboxingStrategy Demand
    arg_ubx_strat = wantToUnboxArg (wo_fam_envs opts) has_inlineable_prag
    has_inlineable_prag = isStableUnfolding (realIdUnfolding fun_id)
                          -- See Note [Do not unpack class dictionaries]

    ret_ubx_strat :: UnboxingStrategy Cpr
    ret_ubx_strat = wantToUnboxResult (wo_fam_envs opts)

    -- Note [Do not split void functions]
    only_one_void_argument
      | [d] <- arg_dmds
      , Just (_, arg_ty1, _) <- splitFunTy_maybe fun_ty
      , isAbsDmd d && isVoidTy arg_ty1
      = True
      | otherwise
      = False

    -- Note [Join points returning functions]
    too_many_args_for_join_point wrap_args
      | Just join_arity <- mb_join_arity
      , wrap_args `lengthExceeds` join_arity
      = WARN(True, text "Unable to worker/wrapper join point with arity " <+>
                     int join_arity <+> text "but" <+>
                     int (length wrap_args) <+> text "args")
        True
      | otherwise
      = False

-- See Note [Limit w/w arity]
isWorkerSmallEnough :: Int -> Int -> [Var] -> Bool
isWorkerSmallEnough max_worker_args old_n_args vars
  = count isId vars <= max old_n_args max_worker_args
    -- We count only Free variables (isId) to skip Type, Kind
    -- variables which have no runtime representation.
    -- Also if the function took 82 arguments before (old_n_args), it's fine if
    -- it takes <= 82 arguments afterwards.

{-
Note [Always do CPR w/w]
~~~~~~~~~~~~~~~~~~~~~~~~
At one time we refrained from doing CPR w/w for thunks, on the grounds that
we might duplicate work.  But that is already handled by CPR analysis,
which doesn't give the CPR property if w/w might waste work: see
Note [CPR for thunks] in GHC.Core.Opt.CprAnal.

And if something *has* been given the CPR property and we don't w/w, it's
a disaster, because then the enclosing function might say it has the CPR
property, but now doesn't and there a cascade of disaster.  A good example
is #5920.

Note [Limit w/w arity]
~~~~~~~~~~~~~~~~~~~~~~~~
Guard against high worker arity as it generates a lot of stack traffic.
A simplified example is #11565#comment:6

Current strategy is very simple: don't perform w/w transformation at all
if the result produces a wrapper with arity higher than -fmax-worker-args
and the number arguments before w/w (see #18122).

It is a bit all or nothing, consider

        f (x,y) (a,b,c,d,e ... , z) = rhs

Currently we will remove all w/w ness entirely. But actually we could
w/w on the (x,y) pair... it's the huge product that is the problem.

Could we instead refrain from w/w on an arg-by-arg basis? Yes, that'd
solve f. But we can get a lot of args from deeply-nested products:

        g (a, (b, (c, (d, ...)))) = rhs

This is harder to spot on an arg-by-arg basis. Previously mkWwStr was
given some "fuel" saying how many arguments it could add; when we ran
out of fuel it would stop w/wing.

Still not very clever because it had a left-right bias.

************************************************************************
*                                                                      *
\subsection{Making wrapper args}
*                                                                      *
************************************************************************

During worker-wrapper stuff we may end up with an unlifted thing
which we want to let-bind without losing laziness.  So we
add a void argument.  E.g.

        f = /\a -> \x y z -> E::Int#    -- E does not mention x,y,z
==>
        fw = /\ a -> \void -> E
        f  = /\ a -> \x y z -> fw realworld

We use the state-token type which generates no code.
-}

mkWorkerArgs :: Bool
             -> [Var]
             -> Type    -- Type of body
             -> ([Var], -- Lambda bound args
                 [Var]) -- Args at call site
mkWorkerArgs fun_to_thunk args res_ty
    | any isId args || not needsAValueLambda
    = (args, args)
    | otherwise
    = (args ++ [voidArgId], args ++ [voidPrimId])
    where
      -- See "Making wrapper args" section above
      needsAValueLambda =
        lifted
        -- We may encounter a levity-polymorphic result, in which case we
        -- conservatively assume that we have laziness that needs preservation.
        -- See #15186.
        || not fun_to_thunk
           -- see Note [Protecting the last value argument]

      -- Might the result be lifted?
      lifted =
        case isLiftedType_maybe res_ty of
          Just lifted -> lifted
          Nothing     -> True

{-
Note [Protecting the last value argument]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the user writes (\_ -> E), they might be intentionally disallowing
the sharing of E. Since absence analysis and worker-wrapper are keen
to remove such unused arguments, we add in a void argument to prevent
the function from becoming a thunk.

The user can avoid adding the void argument with the -ffun-to-thunk
flag. However, this can create sharing, which may be bad in two ways. 1) It can
create a space leak. 2) It can prevent inlining *under a lambda*. If w/w
removes the last argument from a function f, then f now looks like a thunk, and
so f can't be inlined *under a lambda*.

Note [Join points and beta-redexes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Originally, the worker would invoke the original function by calling it with
arguments, thus producing a beta-redex for the simplifier to munch away:

  \x y z -> e => (\x y z -> e) wx wy wz

Now that we have special rules about join points, however, this is Not Good if
the original function is itself a join point, as then it may contain invocations
of other join points:

  join j1 x = ...
  join j2 y = if y == 0 then 0 else j1 y

  =>

  join j1 x = ...
  join $wj2 y# = let wy = I# y# in (\y -> if y == 0 then 0 else jump j1 y) wy
  join j2 y = case y of I# y# -> jump $wj2 y#

There can't be an intervening lambda between a join point's declaration and its
occurrences, so $wj2 here is wrong. But of course, this is easy enough to fix:

  ...
  let join $wj2 y# = let wy = I# y# in let y = wy in if y == 0 then 0 else j1 y
  ...

Hence we simply do the beta-reduction here. (This would be harder if we had to
worry about hygiene, but luckily wy is freshly generated.)

Note [Join points returning functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is crucial that the arity of a join point depends on its *callers,* not its
own syntax. What this means is that a join point can have "extra lambdas":

f :: Int -> Int -> (Int, Int) -> Int
f x y = join j (z, w) = \(u, v) -> ...
        in jump j (x, y)

Typically this happens with functions that are seen as computing functions,
rather than being curried. (The real-life example was GHC.Data.Graph.Ops.addConflicts.)

When we create the wrapper, it *must* be in "eta-contracted" form so that the
jump has the right number of arguments:

f x y = join $wj z' w' = \u' v' -> let {z = z'; w = w'; u = u'; v = v'} in ...
             j (z, w)  = jump $wj z w

(See Note [Join points and beta-redexes] for where the lets come from.) If j
were a function, we would instead say

f x y = let $wj = \z' w' u' v' -> let {z = z'; w = w'; u = u'; v = v'} in ...
            j (z, w) (u, v) = $wj z w u v

Notice that the worker ends up with the same lambdas; it's only the wrapper we
have to be concerned about.

FIXME Currently the functionality to produce "eta-contracted" wrappers is
unimplemented; we simply give up.

************************************************************************
*                                                                      *
\subsection{Coercion stuff}
*                                                                      *
************************************************************************

We really want to "look through" coerces.
Reason: I've seen this situation:

        let f = coerce T (\s -> E)
        in \x -> case x of
                    p -> coerce T' f
                    q -> \s -> E2
                    r -> coerce T' f

If only we w/w'd f, we'd get
        let f = coerce T (\s -> fw s)
            fw = \s -> E
        in ...

Now we'll inline f to get

        let fw = \s -> E
        in \x -> case x of
                    p -> fw
                    q -> \s -> E2
                    r -> fw

Now we'll see that fw has arity 1, and will arity expand
the \x to get what we want.
-}

-- mkWWargs just does eta expansion
-- is driven off the function type and arity.
-- It chomps bites off foralls, arrows, newtypes
-- and keeps repeating that until it's satisfied the supplied arity

mkWWargs :: TCvSubst            -- Freshening substitution to apply to the type
                                --   See Note [Freshen WW arguments]
         -> Type                -- The type of the function
         -> [Demand]     -- Demands and one-shot info for value arguments
         -> UniqSM  ([Var],            -- Wrapper args
                     CoreExpr -> CoreExpr,      -- Wrapper fn
                     CoreExpr -> CoreExpr,      -- Worker fn
                     Type)                      -- Type of wrapper body

mkWWargs subst fun_ty demands
  | null demands
  = return ([], id, id, substTy subst fun_ty)

  | (dmd:demands') <- demands
  , Just (mult, arg_ty, fun_ty') <- splitFunTy_maybe fun_ty
  = do  { uniq <- getUniqueM
        ; let arg_ty' = substScaledTy subst (Scaled mult arg_ty)
              id = mk_wrap_arg uniq arg_ty' dmd
        ; (wrap_args, wrap_fn_args, work_fn_args, res_ty)
              <- mkWWargs subst fun_ty' demands'
        ; return (id : wrap_args,
                  Lam id . wrap_fn_args,
                  apply_or_bind_then work_fn_args (varToCoreExpr id),
                  res_ty) }

  | Just (tv, fun_ty') <- splitForAllTyCoVar_maybe fun_ty
  = do  { uniq <- getUniqueM
        ; let (subst', tv') = cloneTyVarBndr subst tv uniq
                -- See Note [Freshen WW arguments]
        ; (wrap_args, wrap_fn_args, work_fn_args, res_ty)
             <- mkWWargs subst' fun_ty' demands
        ; return (tv' : wrap_args,
                  Lam tv' . wrap_fn_args,
                  apply_or_bind_then work_fn_args (mkTyArg (mkTyVarTy tv')),
                  res_ty) }

  | Just (co, rep_ty) <- topNormaliseNewType_maybe fun_ty
        -- The newtype case is for when the function has
        -- a newtype after the arrow (rare)
        --
        -- It's also important when we have a function returning (say) a pair
        -- wrapped in a  newtype, at least if CPR analysis can look
        -- through such newtypes, which it probably can since they are
        -- simply coerces.

  = do { (wrap_args, wrap_fn_args, work_fn_args, res_ty)
            <-  mkWWargs subst rep_ty demands
       ; let co' = substCo subst co
       ; return (wrap_args,
                  \e -> Cast (wrap_fn_args e) (mkSymCo co'),
                  \e -> work_fn_args (Cast e co'),
                  res_ty) }

  | otherwise
  = WARN( True, ppr fun_ty )                    -- Should not happen: if there is a demand
    return ([], id, id, substTy subst fun_ty)   -- then there should be a function arrow
  where
    -- See Note [Join points and beta-redexes]
    apply_or_bind_then k arg (Lam bndr body)
      = mkCoreLet (NonRec bndr arg) (k body)    -- Important that arg is fresh!
    apply_or_bind_then k arg fun
      = k $ mkCoreApp (text "mkWWargs") fun arg
applyToVars :: [Var] -> CoreExpr -> CoreExpr
applyToVars vars fn = mkVarApps fn vars

mk_wrap_arg :: Unique -> Scaled Type -> Demand -> Id
mk_wrap_arg uniq (Scaled w ty) dmd
  = mkSysLocalOrCoVar (fsLit "w") uniq w ty
       `setIdDemandInfo` dmd

{- Note [Freshen WW arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Wen we do a worker/wrapper split, we must not in-scope names as the arguments
of the worker, else we'll get name capture.  E.g.

   -- y1 is in scope from further out
   f x = ..y1..

If we accidentally choose y1 as a worker argument disaster results:

   fww y1 y2 = let x = (y1,y2) in ...y1...

To avoid this:

  * We use a fresh unique for both type-variable and term-variable binders
    Originally we lacked this freshness for type variables, and that led
    to the very obscure #12562.  (A type variable in the worker shadowed
    an outer term-variable binding.)

  * Because of this cloning we have to substitute in the type/kind of the
    new binders.  That's why we carry the TCvSubst through mkWWargs.

    So we need a decent in-scope set, just in case that type/kind
    itself has foralls.  We get this from the free vars of the RHS of the
    function since those are the only variables that might be captured.
    It's a lazy thunk, which will only be poked if the type/kind has a forall.

    Another tricky case was when f :: forall a. a -> forall a. a->a
    (i.e. with shadowing), and then the worker used the same 'a' twice.
-}

{-
************************************************************************
*                                                                      *
\subsection{Unboxing Strategies for Strictness and CPR}
*                                                                      *
************************************************************************
-}

-- | 'UnboxingStrategy' for strict arguments
wantToUnboxArg :: FamInstEnvs -> Bool -> UnboxingStrategy Demand
-- See Note [Which types are unboxed?]
wantToUnboxArg fam_envs has_inlineable_prag ty dmd
  | isAbsDmd dmd
  = DropAbsent

  | isStrUsedDmd dmd
  , Just (tc, tc_args, co) <- normSplitTyConApp_maybe fam_envs ty
   -- See Note [Which types are unboxed?]
  , Just dc <- tyConSingleAlgDataCon_maybe tc
  , let arity = dataConRepArity dc
  -- See Note [Unpacking arguments with product and polymorphic demands]
  , Just cs <- split_prod_dmd_arity dmd arity
  -- See Note [Do not unpack class dictionaries]
  , not (has_inlineable_prag && isClassPred ty)
  -- See Note [mkWWstr and unsafeCoerce]
  , cs `lengthIs` arity
  = Unbox (DataConPatContext dc tc_args co) cs

  | otherwise
  = StopUnboxing

  where
    split_prod_dmd_arity dmd arity
      -- For seqDmd, it should behave like <S(AAAA)>, for some
      -- suitable arity
      | isSeqDmd dmd        = Just (replicate arity absDmd)
      | _ :* Prod ds <- dmd = Just ds
      | otherwise           = Nothing


-- | 'UnboxingStrategy' for constructed results
wantToUnboxResult :: FamInstEnvs -> UnboxingStrategy Cpr
-- See Note [Which types are unboxed?]
wantToUnboxResult fam_envs ty cpr
  | Just (con_tag, arg_cprs) <- asConCpr cpr
  , Just (tc, tc_args, co) <- normSplitTyConApp_maybe fam_envs ty
  -- See Note [non-algebraic or open body type warning]
  , Just dcs <- tyConAlgDataCons_maybe tc <|> open_body_ty_warning
  , dcs `lengthAtLeast` con_tag -- This might not be true if we import the
                                -- type constructor via a .hs-boot file (#8743)
  , let dc = dcs `getNth` (con_tag - fIRST_TAG)
  , null (dataConExTyCoVars dc) -- no existentials;
                                -- See Note [Which types are unboxed?]
                                -- and GHC.Core.Opt.CprAnal.extendEnvForDataAlt
                                -- where we also check this.
  , all isLinear (dataConInstArgTys dc tc_args)
  -- Deactivates CPR worker/wrapper splits on constructors with non-linear
  -- arguments, for the moment, because they require unboxed tuple with variable
  -- multiplicity fields.
  = Unbox (DataConPatContext dc tc_args co) arg_cprs

  | otherwise
  = StopUnboxing

  where
    open_body_ty_warning = WARN( True, text "wantToUnboxResult: non-algebraic or open body type" <+> ppr ty ) Nothing

isLinear :: Scaled a -> Bool
isLinear (Scaled w _ ) =
  case w of
    One -> True
    _ -> False

{- Note [Which types are unboxed?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Worker/wrapper will unbox

  1. A strict data type argument, that
       * is an algebraic data type (not a newtype)
       * has a single constructor (thus is a "product")
       * that may bind existentials
     We can transform
     > f (D @ex a b) = e
     to
     > $wf @ex a b = e
     via 'mkWWstr'.

  2. The constructed result of a function, if
       * its type is an algebraic data type (not a newtype)
       * (might have multiple constructors, in contrast to (1))
       * the applied data constructor *does not* bind existentials
     We can transform
     > f x y = let ... in D a b
     to
     > $wf x y = let ... in (# a, b #)
     via 'mkWWcpr_start'.

     NB: We don't allow existentials for CPR W/W, because we don't have unboxed
     dependent tuples (yet?). Otherwise, we could transform
     > f x y = let ... in D @ex (a :: ..ex..) (b :: ..ex..)
     to
     > $wf xopts want_to_unbox = let ... in (# @ex, (a :: ..ex..), (b :: ..ex..) #)

The respective tests are in 'wantToUnboxArg' and
'wantToUnboxResult', respectively.

Note that the data constructor /can/ have evidence arguments: equality
constraints, type classes etc.  So it can be GADT.  These evidence
arguments are simply value arguments, and should not get in the way.

Note [Unpacking arguments with product and polymorphic demands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The argument is unpacked in a case if it has a product type and has a
strict *and* used demand put on it. I.e., arguments, with demands such
as the following ones:

   <S,U(U, L)>
   <S(L,S),U>

will be unpacked, but

   <S,U> or <B,U>

will not, because the pieces aren't used. This is quite important otherwise
we end up unpacking massive tuples passed to the bottoming function. Example:

        f :: ((Int,Int) -> String) -> (Int,Int) -> a
        f g pr = error (g pr)

        main = print (f fst (1, error "no"))

Does 'main' print "error 1" or "error no"?  We don't really want 'f'
to unbox its second argument.  This actually happened in GHC's onwn
source code, in Packages.applyPackageFlag, which ended up un-boxing
the enormous DynFlags tuple, and being strict in the
as-yet-un-filled-in unitState files.

Note [Do not unpack class dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have
   f :: Ord a => [a] -> Int -> a
   {-# INLINABLE f #-}
and we worker/wrapper f, we'll get a worker with an INLINABLE pragma
(see Note [Worker-wrapper for INLINABLE functions] in GHC.Core.Opt.WorkWrap),
which can still be specialised by the type-class specialiser, something like
   fw :: Ord a => [a] -> Int# -> a

BUT if f is strict in the Ord dictionary, we might unpack it, to get
   fw :: (a->a->Bool) -> [a] -> Int# -> a
and the type-class specialiser can't specialise that. An example is #6056.

But in any other situation a dictionary is just an ordinary value,
and can be unpacked.  So we track the INLINABLE pragma, and switch
off the unpacking in mkWWstr_one (see the isClassPred test).

Historical note: #14955 describes how I got this fix wrong the first time.

Note [mkWWstr and unsafeCoerce]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
By using unsafeCoerce, it is possible to make the number of demands fail to
match the number of constructor arguments; this happened in #8037.
If so, the worker/wrapper split doesn't work right and we get a Core Lint
bug.  The fix here is simply to decline to do w/w if that happens.

Note [non-algebraic or open body type warning]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are a few cases where the W/W transformation is told that something
returns a constructor, but the type at hand doesn't really match this. One
real-world example involves unsafeCoerce:
  foo = IO a
  foo = unsafeCoerce c_exit
  foreign import ccall "c_exit" c_exit :: IO ()
Here CPR will tell you that `foo` returns a () constructor for sure, but trying
to create a worker/wrapper for type `a` obviously fails.
(This was a real example until ee8e792  in libraries/base.)

It does not seem feasible to avoid all such cases already in the analyser (and
after all, the analysis is not really wrong), so we simply do nothing here in
mkWWcpr_one. But we still want to emit warning with -DDEBUG, to hopefully catch
other cases where something went avoidably wrong.

This warning also triggers for the stream fusion library within `text`.
We can'easily W/W constructed results like `Stream` because we have no simple
way to express existential types in the worker's type signature.

************************************************************************
*                                                                      *
\subsection{Strictness stuff}
*                                                                      *
************************************************************************
-}

mkWWstr :: WwOpts
        -> UnboxingStrategy Demand
        -> [Var]                                -- Wrapper args; have their demand info on them
                                                --  *Includes type variables*
        -> UniqSM (Bool,                        -- Is this useful
                   [Var],                       -- Worker args
                   CoreExpr -> CoreExpr,        -- Wrapper body, lacking the worker call
                                                -- and without its lambdas
                                                -- This fn adds the unboxing

                   CoreExpr -> CoreExpr)        -- Worker body, lacking the original body of the function,
                                                -- and lacking its lambdas.
                                                -- This fn does the reboxing
mkWWstr opts want_to_unbox args
  = go args
  where
    go_one arg = mkWWstr_one opts want_to_unbox arg

    go []           = return (False, [], nop_fn, nop_fn)
    go (arg : args) = do { (useful1, args1, wrap_fn1, work_fn1) <- go_one arg
                         ; (useful2, args2, wrap_fn2, work_fn2) <- go args
                         ; return ( useful1 || useful2
                                  , args1 ++ args2
                                  , wrap_fn1 . wrap_fn2
                                  , work_fn1 . work_fn2) }

----------------------
-- mkWWstr_one wrap_arg = (useful, work_args, wrap_fn, work_fn)
--   *  wrap_fn assumes wrap_arg is in scope,
--        brings into scope work_args (via cases)
--   * work_fn assumes work_args are in scope,
--        brings into scope wrap_arg (via lets)
-- See Note [Worker/wrapper for Strictness and Absence]
mkWWstr_one :: WwOpts
            -> UnboxingStrategy Demand
            -> Var
            -> UniqSM (Bool, [Var], CoreExpr -> CoreExpr, CoreExpr -> CoreExpr)
mkWWstr_one opts want_to_unbox arg =
  case want_to_unbox (idType arg) (idDemandInfo arg) of
     _ | isTyVar arg -> do_nothing

     DropAbsent
       | Just work_fn <- mk_absent_let opts arg (idDemandInfo arg)
         -- Absent case.  We can't always handle absence for arbitrary
         -- unlifted types, so we need to choose just the cases we can
         -- (that's what mk_absent_let does)
       -> return (True, [], nop_fn, work_fn)

     Unbox dcpc cs -> unbox_one_arg opts want_to_unbox arg cs dcpc

     _ -> do_nothing -- Other cases, like StopUnboxing

  where
    do_nothing = return (False, [arg], nop_fn, nop_fn)

unbox_one_arg :: WwOpts
          -> UnboxingStrategy Demand
          -> Var
          -> [Demand]
          -> DataConPatContext
          -> UniqSM (Bool, [Var], CoreExpr -> CoreExpr, CoreExpr -> CoreExpr)
unbox_one_arg opts want_to_unbox arg cs
          DataConPatContext { dcpc_dc = dc, dcpc_tc_args = tc_args
                            , dcpc_co = co }
  = do { pat_bndrs_uniqs <- getUniquesM
       ; let ex_name_fss = map getOccFS $ dataConExTyCoVars dc
             (ex_tvs', arg_ids) =
               dataConRepFSInstPat (ex_name_fss ++ repeat ww_prefix) pat_bndrs_uniqs (idMult arg) dc tc_args
             -- See Note [Add demands for strict constructors]
             cs'       = addDataConStrictness dc cs
             arg_ids'  = zipWithEqual "unbox_one_arg" setIdDemandInfo arg_ids cs'
             unbox_fn  = mkUnpackCase (Var arg) co (idMult arg)
                                      dc (ex_tvs' ++ arg_ids')
             arg_no_unf = zapStableUnfolding arg
                          -- See Note [Zap unfolding when beta-reducing]
                          -- in GHC.Core.Opt.Simplify; and see #13890
             rebox_fn   = Let (NonRec arg_no_unf con_app)
             con_app    = mkConApp2 dc tc_args (ex_tvs' ++ arg_ids') `mkCast` mkSymCo co
       ; (_, worker_args, wrap_fn, work_fn) <- mkWWstr opts want_to_unbox (ex_tvs' ++ arg_ids')
       ; return (True, worker_args, unbox_fn . wrap_fn, work_fn . rebox_fn) }
                          -- Don't pass the arg, rebox instead

----------------------
nop_fn :: CoreExpr -> CoreExpr
nop_fn body = body

addDataConStrictness :: DataCon -> [Demand] -> [Demand]
-- See Note [Add demands for strict constructors]
addDataConStrictness con ds
  = zipWithEqual "addDataConStrictness" add ds strs
  where
    strs = dataConRepStrictness con
    add dmd str | isMarkedStrict str = strictifyDmd dmd
                | otherwise          = dmd

{- Note [Worker/wrapper for Strictness and Absence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The worker-wrapper transformation, mkWWstr_one, takes into account
several possibilities to decide if the function is worthy for
splitting:

1. If an argument is absent, it would be silly to pass it to
   the worker.  Hence the isAbsDmd case.  This case must come
   first because a demand like <S,A> or <B,A> is possible.
   E.g. <B,A> comes from a function like
       f x = error "urk"
   and <S,A> can come from Note [Add demands for strict constructors]

2. If the argument is evaluated strictly, and we can split the
   product demand (splitProdDmd_maybe), then unbox it and w/w its
   pieces.  For example

    f :: (Int, Int) -> Int
    f p = (case p of (a,b) -> a) + 1
  is split to
    f :: (Int, Int) -> Int
    f p = case p of (a,b) -> $wf a

    $wf :: Int -> Int
    $wf a = a + 1

  and
    g :: Bool -> (Int, Int) -> Int
    g c p = case p of (a,b) ->
               if c then a else b
  is split to
   g c p = case p of (a,b) -> $gw c a b
   $gw c a b = if c then a else b

2a But do /not/ split if the components are not used; that is, the
   usage is just 'Used' rather than 'UProd'. In this case
   splitProdDmd_maybe returns Nothing.  Otherwise we risk decomposing
   a massive tuple which is barely used.  Example:

        f :: ((Int,Int) -> String) -> (Int,Int) -> a
        f g pr = error (g pr)

        main = print (f fst (1, error "no"))

   Here, f does not take 'pr' apart, and it's stupid to do so.
   Imagine that it had millions of fields. This actually happened
   in GHC itself where the tuple was DynFlags

3. A plain 'seqDmd', which is head-strict with usage UHead, can't
   be split by splitProdDmd_maybe.  But we want it to behave just
   like U(AAAA) for suitable number of absent demands. So we have
   a special case for it, with arity coming from the data constructor.

Note [Worker-wrapper for bottoming functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used not to split if the result is bottom.
[Justification:  there's no efficiency to be gained.]

But it's sometimes bad not to make a wrapper.  Consider
        fw = \x# -> let x = I# x# in case e of
                                        p1 -> error_fn x
                                        p2 -> error_fn x
                                        p3 -> the real stuff
The re-boxing code won't go away unless error_fn gets a wrapper too.
[We don't do reboxing now, but in general it's better to pass an
unboxed thing to f, and have it reboxed in the error cases....]

Note [Add demands for strict constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this program (due to Roman):

    data X a = X !a

    foo :: X Int -> Int -> Int
    foo (X a) n = go 0
     where
       go i | i < n     = a + go (i+1)
            | otherwise = 0

We want the worker for 'foo' too look like this:

    $wfoo :: Int# -> Int# -> Int#

with the first argument unboxed, so that it is not eval'd each time
around the 'go' loop (which would otherwise happen, since 'foo' is not
strict in 'a').  It is sound for the wrapper to pass an unboxed arg
because X is strict, so its argument must be evaluated.  And if we
*don't* pass an unboxed argument, we can't even repair it by adding a
`seq` thus:

    foo (X a) n = a `seq` go 0

because the seq is discarded (very early) since X is strict!

So here's what we do

* We leave the demand-analysis alone.  The demand on 'a' in the
  definition of 'foo' is <L, U(U)>; the strictness info is Lazy
  because foo's body may or may not evaluate 'a'; but the usage info
  says that 'a' is unpacked and its content is used.

* During worker/wrapper, if we unpack a strict constructor (as we do
  for 'foo'), we use 'addDataConStrictness' to bump up the strictness on
  the strict arguments of the data constructor.

* That in turn means that, if the usage info supports doing so
  (i.e. splitProdDmd_maybe returns Just), we will unpack that argument
  -- even though the original demand (e.g. on 'a') was lazy.

* What does "bump up the strictness" mean?  Just add a head-strict
  demand to the strictness!  Even for a demand like <L,A> we can
  safely turn it into <S,A>; remember case (1) of
  Note [Worker/wrapper for Strictness and Absence].

The net effect is that the w/w transformation is more aggressive about
unpacking the strict arguments of a data constructor, when that
eagerness is supported by the usage info.

There is the usual danger of reboxing, which as usual we ignore. But
if X is monomorphic, and has an UNPACK pragma, then this optimisation
is even more important.  We don't want the wrapper to rebox an unboxed
argument, and pass an Int to $wfoo!

This works in nested situations like

    data family Bar a
    data instance Bar (a, b) = BarPair !(Bar a) !(Bar b)
    newtype instance Bar Int = Bar Int

    foo :: Bar ((Int, Int), Int) -> Int -> Int
    foo f k = case f of BarPair x y ->
              case burble of
                 True -> case x of
                           BarPair p q -> ...
                 False -> ...

The extra eagerness lets us produce a worker of type:
     $wfoo :: Int# -> Int# -> Int# -> Int -> Int
     $wfoo p# q# y# = ...

even though the `case x` is only lazily evaluated.

--------- Historical note ------------
We used to add data-con strictness demands when demand analysing case
expression. However, it was noticed in #15696 that this misses some cases. For
instance, consider the program (from T10482)

    data family Bar a
    data instance Bar (a, b) = BarPair !(Bar a) !(Bar b)
    newtype instance Bar Int = Bar Int

    foo :: Bar ((Int, Int), Int) -> Int -> Int
    foo f k =
      case f of
        BarPair x y -> case burble of
                          True -> case x of
                                    BarPair p q -> ...
                          False -> ...

We really should be able to assume that `p` is already evaluated since it came
from a strict field of BarPair. This strictness would allow us to produce a
worker of type:

    $wfoo :: Int# -> Int# -> Int# -> Int -> Int
    $wfoo p# q# y# = ...

even though the `case x` is only lazily evaluated

Indeed before we fixed #15696 this would happen since we would float the inner
`case x` through the `case burble` to get:

    foo f k =
      case f of
        BarPair x y -> case x of
                          BarPair p q -> case burble of
                                          True -> ...
                                          False -> ...

However, after fixing #15696 this could no longer happen (for the reasons
discussed in ticket:15696#comment:76). This means that the demand placed on `f`
would then be significantly weaker (since the False branch of the case on
`burble` is not strict in `p` or `q`).

Consequently, we now instead account for data-con strictness in mkWWstr_one,
applying the strictness demands to the final result of DmdAnal. The result is
that we get the strict demand signature we wanted even if we can't float
the case on `x` up through the case on `burble`.

************************************************************************
*                                                                      *
         Type scrutiny that is specific to demand analysis
*                                                                      *
************************************************************************
-}

findTypeShape :: FamInstEnvs -> Type -> TypeShape
-- Uncover the arrow and product shape of a type
-- The data type TypeShape is defined in GHC.Types.Demand
-- See Note [Trimming a demand to a type] in GHC.Core.Opt.DmdAnal
findTypeShape fam_envs ty
  = go (setRecTcMaxBound 2 initRecTc) ty
       -- You might think this bound of 2 is low, but actually
       -- I think even 1 would be fine.  This only bites for recursive
       -- product types, which are rare, and we really don't want
       -- to look deep into such products -- see #18034
  where
    go rec_tc ty
       | Just (_, _, res) <- splitFunTy_maybe ty
       = TsFun (go rec_tc res)

       | Just (tc, tc_args)  <- splitTyConApp_maybe ty
       = go_tc rec_tc tc tc_args

       | Just (_, ty') <- splitForAllTyCoVar_maybe ty
       = go rec_tc ty'

       | otherwise
       = TsUnk

    go_tc rec_tc tc tc_args
       | Just (_, rhs, _) <- topReduceTyFamApp_maybe fam_envs tc tc_args
       = go rec_tc rhs

       | Just con <- tyConSingleAlgDataCon_maybe tc
       , Just rec_tc <- if isTupleTyCon tc
                        then Just rec_tc
                        else checkRecTc rec_tc tc
         -- We treat tuples specially because they can't cause loops.
         -- Maybe we should do so in checkRecTc.
         -- The use of 'dubiousDataConInstArgTys' is OK, since this
         -- function performs no substitution at all, hence the uniques
         -- don't matter.
       = TsProd (map (go rec_tc) (dubiousDataConInstArgTys con tc_args))

       | Just (ty', _) <- instNewTyCon_maybe tc tc_args
       , Just rec_tc <- checkRecTc rec_tc tc
       = go rec_tc ty'

       | otherwise
       = TsUnk

-- | Exactly 'dataConInstArgTys', but lacks the (ASSERT'ed) precondition that
-- the 'DataCon' may not have existentials. The lack of cloning the existentials
-- compared to 'dataConInstExAndArgVars' makes this function \"dubious\";
-- only use it where type variables aren't substituted for!
dubiousDataConInstArgTys :: DataCon -> [Type] -> [Type]
dubiousDataConInstArgTys dc tc_args = arg_tys
  where
    univ_tvs = dataConUnivTyVars dc
    ex_tvs   = dataConExTyCoVars dc
    subst    = extendTCvInScopeList (zipTvSubst univ_tvs tc_args) ex_tvs
    arg_tys  = map (substTy subst . scaledThing) (dataConRepArgTys dc)

{-
************************************************************************
*                                                                      *
\subsection{CPR stuff}
*                                                                      *
************************************************************************
-}

mkWWcpr_start
  :: WwOpts
  -> UnboxingStrategy Cpr
  -> Type                              -- function body
  -> Cpr                               -- CPR analysis results
  -> UniqSM (Bool,                     -- Is w/w'ing useful?
             CoreExpr -> CoreExpr,     -- New wrapper
             CoreExpr -> CoreExpr,     -- New worker
             Type)                     -- Type of worker's body
-- ^ Entrypoint to CPR W/W. See Note [Worker/wrapper for CPR] for an overview.
mkWWcpr_start opts want_to_unbox body_ty body_cpr
  | not (wo_cpr_anal opts) = return (False, id, id, body_ty)
  | otherwise = do
    -- Part (1)
    res_bndr <- mk_res_bndr body_ty body_cpr
    let bind_res_bndr body scope = mkDefaultCase body res_bndr scope
        deref_res_bndr           = Var res_bndr

    -- Part (2)
    (useful, fromOL -> transit_vars, wrap_build_res, work_unpack_res) <-
      mkWWcpr_one opts want_to_unbox res_bndr body_cpr

    -- Part (3)
    let (unbox_transit_tup, transit_tup) = move_transit_vars transit_vars

    -- Stacking unboxer (work_fn) and builder (wrap_fn) together
    let wrap_fn = unbox_transit_tup (wrap_build_res deref_res_bndr)     -- 3 2 1
        work_fn body = bind_res_bndr body (work_unpack_res transit_tup) -- 1 2 3
        work_body_ty = exprType transit_tup
    return $ if not useful
                then (False, nop_fn, nop_fn, body_ty)
                else (True, wrap_fn, work_fn, work_body_ty)
  where
    mk_res_bndr :: Type -> Cpr -> UniqSM Id
    mk_res_bndr body_ty body_cpr = do
      -- See Note [Linear types and CPR]
      bndr <- mkSysLocalOrCoVarM ww_prefix cprCaseBndrMult body_ty
      -- See Note [Record evaluated-ness in worker/wrapper]
      pure $ setCaseBndrEvald MarkedStrict bndr
               `setIdCprInfo` mkCprSig 0 body_cpr -- so that we see that it terminates

-- | What part (2) of Note [Worker/wrapper for CPR] collects.
--
--   1. A 'Bool' capturing whether the transformation did anything useful.
--   2. The list of transit variables (see the Note).
--   3. The result builder expression for the wrapper
--   4. The result unpacking expression for the worker
type CprWwResult = (Bool, OrdList Var, CoreExpr -> CoreExpr, CoreExpr -> CoreExpr)

mkWWcpr :: WwOpts -> UnboxingStrategy Cpr -> [Id] -> [Cpr] -> UniqSM CprWwResult
mkWWcpr opts want_to_unbox vars cprs = do
  -- No existentials in 'vars'. 'wantToUnboxResult' should have checked that.
  MASSERT2( not (any isTyVar vars), ppr vars $$ ppr cprs )
  MASSERT2( equalLength vars cprs, ppr vars $$ ppr cprs )
  (usefuls, varss, wrap_build_ress, work_unpack_ress) <-
    unzip4 <$> zipWithM (mkWWcpr_one opts want_to_unbox) vars cprs
  return ( or usefuls
         , concatOL varss
         , foldl' (.) id wrap_build_ress
         , foldl' (.) id work_unpack_ress )

mkWWcpr_one :: WwOpts -> UnboxingStrategy Cpr -> Id -> Cpr -> UniqSM CprWwResult
-- ^ See if we want to unbox the result and hand off to 'unbox_one_result'.
mkWWcpr_one opts want_to_unbox res_bndr cpr
  | ASSERT( not (isTyVar res_bndr) ) True
  , Unbox dcpc arg_cprs <- want_to_unbox (idType res_bndr) cpr
  = unbox_one_result opts want_to_unbox res_bndr arg_cprs dcpc
  | otherwise
  = return (False, unitOL res_bndr, nop_fn, nop_fn)

unbox_one_result
  :: WwOpts -> UnboxingStrategy Cpr -> Id -> [Cpr] -> DataConPatContext
  -> UniqSM CprWwResult
-- ^ Implements the main bits of part (2) of Note [Worker/wrapper for CPR]
unbox_one_result opts want_to_unbox res_bndr arg_cprs
                 DataConPatContext { dcpc_dc = dc, dcpc_tc_args = tc_args
                                   , dcpc_co = co } = do
  -- unboxer (free in `res_bndr`):       |   builder (binds `res_bndr`):
  --   ( case res_bndr of (i, j) -> )    |     ( let j = I# b in          )
  --   ( case i of I# a ->          )    |     ( let i = I# a in          )
  --   ( case j of I# b ->          )    |     ( let res_bndr = (i, j) in )
  --   ( <hole>                     )    |     ( <hole>                   )
  pat_bndrs_uniqs <- getUniquesM
  let (_exs, arg_ids) =
        dataConRepFSInstPat (repeat ww_prefix) pat_bndrs_uniqs cprCaseBndrMult dc tc_args
  MASSERT( null _exs ) -- Should have been caught by wantToUnboxResult

  let -- transfer cpr info to field binders
      arg_ids' = zipWithEqual "unbox_one_result" (flip setIdCprInfo . mkCprSig 0) arg_cprs arg_ids
      -- con_app = (C a b |> sym co)
      con_app = mkConApp2 dc tc_args arg_ids' `mkCast` mkSymCo co
      -- this_wrap_build_res body = (let res_bndr = C a b |> sym co in <body>[r])
      this_wrap_build_res  = Let (NonRec res_bndr con_app)
      -- this_work_unbox_res alt = (case res_bndr |> co of C a b -> <alt>[a,b])
      this_work_unbox_res = mkUnpackCase (Var res_bndr) co cprCaseBndrMult dc arg_ids'

  (nested_useful, transit_vars, wrap_build_res, work_unbox_res) <-
    mkWWcpr opts want_to_unbox arg_ids' arg_cprs

  -- Don't try to WW an unboxed tuple return type when there's nothing inside
  -- to unbox further.
  return $ if isUnboxedTupleDataCon dc && not nested_useful
              then ( False, unitOL res_bndr, nop_fn, nop_fn )
              else ( True
                   , transit_vars
                   , wrap_build_res . this_wrap_build_res
                   , this_work_unbox_res . work_unbox_res
                   )

-- | Implements part (3) of Note [Worker/wrapper for CPR].
--
-- If `move_transit_vars [a,b] = (unbox, tup)` then
--     * `a` and `b` are the *transit vars* to be returned from the worker
--       to the wrapper
--     * `unbox scrut alt = (case <scrut> of (# a, b #) -> <alt>)`
--     * `tup = (# a, b #)`
-- Special case: If there's only a single var `a` which is known
-- to be cheap to evaluate, then there's no need for a `(# a #)`
-- singleton tuple and we can just pass `a` around.
move_transit_vars :: [Id] -> (CoreExpr -> CoreExpr -> CoreExpr, CoreExpr)
move_transit_vars vars
  | [var] <- vars
  , let var_ty = idType var
  , isUnliftedType var_ty || whnf_term var == Terminates
  -- See Note [No unboxed tuple for single, unlifted transit var]
  --   * Wrapper: `unbox scrut alt = (case <scrut> of a -> <alt>)`
  --   * Worker:  `tup = a`
  = ( \build_res wkr_call -> mkDefaultCase wkr_call var build_res
    , varToCoreExpr var ) -- varToCoreExpr important here: var can be a coercion
                          -- Lacking this caused #10658
  | otherwise
  -- The general case: Just return an unboxed tuple from the worker
  --   * Wrapper: `unbox scrut alt = (case <scrut> of (# a, b #) -> <alt>)`
  --   * Worker:  `tup = (# a, b #)`
  = ( \build_res wkr_call -> mkSingleAltCase wkr_call case_bndr
                                    (DataAlt tup_con) vars build_res
    , ubx_tup_app )
  where
    -- | Whether Termination analysis says that `v` terminates quickly
    whnf_term v = fst $ forceCprTy seqDmd $ getCprSig $ idCprInfo $ v
    ubx_tup_app = mkCoreUbxTup (map idType vars) (map varToCoreExpr vars)
    tup_con     = tupleDataCon Unboxed (length vars)
    -- See also Note [Linear types and CPR]
    case_bndr   = mkWildValBinder cprCaseBndrMult (exprType ubx_tup_app)


{- Note [Worker/wrapper for CPR]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'mkWWcpr_start' is the entry-point to the worker/wrapper transformation that
exploits CPR info. Here's an example:
```
  f :: ... -> (Int, Int)
  f ... = <body>
```
Let's assume the CPR info `body_cpr` for the body of `f` says
"unbox the pair and its components" and `body_ty` is the type of the function
body `body` (i.e., `(Int, Int)`). Then `mkWWcpr_start body_ty body_cpr` returns

  * A result-unpacking expression for the worker, with a hole for the fun body:
    ```
      unpack body = ( case <body> of r __DEFAULT -> )    -- (1)
                    ( case r of (i, j) ->           )    -- (2)
                    ( case i of I# a ->             )    -- (2)
                    ( case j of I# b ->             )    -- (2)
                    ( (# a, b #)                    )    -- (3)
    ```
  * A result-building expression for the wrapper, with a hole for the worker call:
    ```
      build wkr_call = ( case <wkr_call> of (# a, b #) -> )    -- (3)
                       ( let j = I# b in                  )    -- (2)
                       ( let i = I# a in                  )    -- (2)
                       ( let r = (i, j) in                )    -- (2)
                       ( r                                )    -- (1)
    ```
  * The result type of the worker, e.g., `(# Int#, Int# #)` above.

To achieve said transformation, 'mkWWcpr_start'

  1. First allocates a fresh result binder `r`, giving a name to the `body`
     expression and contributing part (1) of the unpacker and builder.
  2. Then it delegates to 'mkWWcpr_one', which recurses into all result fields
     to unbox, contributing the parts marked with (2). Crucially, it knows
     what belongs in the case scrutinee through the communicated Id `r`: The
     unpacking expression will be free in that variable.
     (This is a similar contract as that of 'mkWWstr_one' for strict args.)
  3. 'mkWWstr_one' produces a bunch of *transit vars*: Those result variables
     that have to be transferred from the worker to the wrapper, where the
     constructed result can be rebuild, `a` and `b` above. Part (3) is
     responsible for tupling them up in the worker and taking the tuple apart
     in the wrapper. This is implemented in 'move_transit_vars'.

Note [No unboxed tuple for single, unlifted transit var]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If there's only a single, unlifted transit var (Note [Worker/wrapper for CPR]),
we don't need to allocate an unboxed singleton tuple for that and can return
the unlifted thing directly. E.g.
```
  f :: Int -> Int
  f x = x+1
```
We certainly want `$wf :: Int# -> Int#`, not `$wf :: Int# -> (# Int# #)`.
This is OK as long as we know that evaluation of the returned thing terminates
quickly, as is the case for fields of unlifted type like `Int#`.

But more generally, this is also true for *lifted* types that terminate quickly!
Consider from `T18109`:
```
  data F = F (Int -> Int)
  f :: Int -> F
  f n = F (+n)

  data T = T (Int, Int)
  g :: T -> T
  g t@(T p) = p `seq` t

  data U = U ![Int]
  h :: Int -> U
  h n = U [0..n]
```
Both worker should not wrap the fields in singleton tuples, because they are
already evaluated. For `g`, we manage not to by looking at the termination
information of `p`, which reflects that it has been seq'd.
For `f`, rapid termination analysis should see that `(+n)` terminates rapidly
and thus omit the singleton tuple.
For `h`, we also have to consider the strictness of the field, but in contrast
to Note [Add demands for strict constructors], that may already happen in
'cprTransformDataConSig', so the CPR info should reflect that.

************************************************************************
*                                                                      *
\subsection{Utilities}
*                                                                      *
************************************************************************

Note [Profiling and unpacking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the original function looked like
        f = \ x -> {-# SCC "foo" #-} E

then we want the CPR'd worker to look like
        \ x -> {-# SCC "foo" #-} (case E of I# x -> x)
and definitely not
        \ x -> case ({-# SCC "foo" #-} E) of I# x -> x)

This transform doesn't move work or allocation
from one cost centre to another.

Later [SDM]: presumably this is because we want the simplifier to
eliminate the case, and the scc would get in the way?  I'm ok with
including the case itself in the cost centre, since it is morally
part of the function (post transformation) anyway.

Note [Linear types and CPR]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Remark on linearity: in both the case of the wrapper and the worker,
we build a linear case to unpack constructed products. All the
multiplicity information is kept in the constructors (both C and (#, #)).
In particular (#,#) is parametrised by the multiplicity of its fields.
Specifically, in this instance, the multiplicity of the fields of (#,#)
is chosen to be the same as those of C.

Note [Absent errors]
~~~~~~~~~~~~~~~~~~~~
Consider
  data T = MkT [Int] [Int] ![Int]
  f :: T -> Int# -> blah
  f ps w = case ps of MkT xs _ _ -> <body mentioning xs>
Then f gets a strictness sig of <S(L,A,A)><A>. We make worker $wf thus:

$wf :: [Int] -> blah
$wf xs = case ps of MkT xs _ _ -> <body mentioning xs>
  where
    ys = absentError "ys :: [Int]"
    zs = LitRubbish True
    ps = MkT xs ys zs
    w  = 0#

We make a let-binding for Absent arguments, such as ys and w, that are not even
passed to the worker. They should, of course, never be used. We distinguish four
cases:

1. Ordinary boxed, lifted arguments, like 'ys' We make a new binding for Ids
   that are marked absent, thus
      let ys = absentError "ys :: [Int]"
   The idea is that this binding will never be used; but if it
   buggily is used we'll get a runtime error message.

2. Boxed, lifted types, with a strict demand, like 'zs'.  You may ask: how the
   demand be both absent and strict?  That's exactly what happens for 'zs': it
   is not used, so its demand is Absent, but then during w/w, in
   addDataConStrictness, we strictify the demand.  So it gets cardinality C_10,
   the empty interval.

   We don't want to use an error-thunk for 'zs' because MkT's third argument has
   a bang, and hence should be always evaluated. This turned out to be
   important when fixing #16970, which establishes the invariant that strict
   constructor arguments are always evaluated. So we use LitRubbish instead
   of an error thunk -- see #19133.

   These first two cases are distinguished by isStrictDmd in lifted_rhs.

3. Unboxed types, like 'w', with a type like Float#, Int#. Coping with absence
   for unboxed types is important; see, for example, #4306 and #15627.  We
   simply find a suitable literal, using Literal.absentLiteralOf.  We don't have
   literals for every primitive type, so the function is partial.

4. Boxed, unlifted types, like (Array# t).  We can't use absentError because
   unlifted bindings ares strict.  So we use LitRubbish, which we need to apply
   to the required type.

Case (2) and (4) crucially use LitRubbish as the placeholder: see Note [Rubbish
literals] in GHC.Types.Literal.  We could do that in case (1) as well, but we
get slightly better self-checking with an error thunk.

Suppose we use LitRubbish and absence analysis is Wrong, so that the "absent"
value is used after all.  Then in case (2) we could get a seg-fault, because we
may have replaced, say, a [Either Int Bool] by (), and that will fail if we do
case analysis on it.  Similarly with boxed unlifted types, case (4).

In case (3), if absence analysis is wrong we could conceivably get an exception,
from a divide-by-zero with the absent value.  But it's very unlikely.

Only in case (1) can we guarantee a civilised runtime error.  Not much we can do
about this; we really rely on absence analysis to be correct.


Historical note: I did try the experiment of using an error thunk for unlifted
things too, relying on the simplifier to drop it as dead code.  But this is
fragile

 - It fails when profiling is on, which disables various optimisations

 - It fails when reboxing happens. E.g.
      data T = MkT Int Int#
      f p@(MkT a _) = ...g p....
   where g is /lazy/ in 'p', but only uses the first component.  Then
   'f' is /strict/ in 'p', and only uses the first component.  So we only
   pass that component to the worker for 'f', which reconstructs 'p' to
   pass it to 'g'.  Alas we can't say
       ...f (MkT a (absentError Int# "blah"))...
   because `MkT` is strict in its Int# argument, so we get an absentError
   exception when we shouldn't.  Very annoying!

Note [Record evaluated-ness in worker/wrapper]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

   data T = MkT !Int Int

   f :: T -> T
   f x = e

and f's is strict, and has the CPR property.  The we are going to generate
this w/w split

   f x = case x of
           MkT x1 x2 -> case $wf x1 x2 of
                           (# r1, r2 #) -> MkT r1 r2

   $wfw x1 x2 = let x = MkT x1 x2 in
                case e of
                  MkT r1 r2 -> (# r1, r2 #)

Note that

* In the worker $wf, inside 'e' we can be sure that x1 will be
  evaluated (it came from unpacking the argument MkT.  But that's no
  immediately apparent in $wf

* In the wrapper 'f', which we'll inline at call sites, we can be sure
  that 'r1' has been evaluated (because it came from unpacking the result
  MkT.  But that is not immediately apparent from the wrapper code.

Missing these facts isn't unsound, but it loses possible future
opportunities for optimisation.

Solution: use setCaseBndrEvald when creating
 (A) The arg binders x1,x2 in mkWstr_one
         See #13077, test T13077
 (B) The result binders r1,r2 in mkWWcpr_one_help
         See Trace #13077, test T13077a
         And #13027 comment:20, item (4)
to record that the relevant binder is evaluated.

-}

mkUnpackCase ::  CoreExpr -> Coercion -> Mult -> DataCon -> [Id] -> CoreExpr -> CoreExpr
-- (mkUnpackCase e co Con args body)
--      returns
-- case e |> co of _dead { Con args -> body }
mkUnpackCase (Tick tickish e) co mult con args body   -- See Note [Profiling and unpacking]
  = Tick tickish (mkUnpackCase e co mult con args body)
mkUnpackCase scrut co mult boxing_con unpk_args body
  = mkSingleAltCase casted_scrut bndr
                    (DataAlt boxing_con) unpk_args body
  where
    casted_scrut = scrut `mkCast` co
    bndr = mkWildValBinder mult (exprType casted_scrut)

-- | The multiplicity of a case binder unboxing a constructed result.
-- See Note [Linear types and CPR]
cprCaseBndrMult :: Mult
cprCaseBndrMult = One

-- | Tries to find a suitable dummy RHS to bind the given absent identifier to.
--
-- If @mk_absent_let _ id == Just wrap@, then @wrap e@ will wrap a let binding
-- for @id@ with that RHS around @e@. Otherwise, there could no suitable RHS be
-- found (currently only happens for bindings of 'VecRep' representation).
mk_absent_let :: WwOpts -> Id -> Demand -> Maybe (CoreExpr -> CoreExpr)
mk_absent_let opts arg dmd

  -- The lifted case: Bind 'absentError'
  -- See Note [Absent errors]
  | not (isUnliftedType arg_ty)
  = Just (Let (NonRec lifted_arg lifted_rhs))
  -- The 'UnliftedRep' (because polymorphic) case: Bind @__RUBBISH \@arg_ty@
  -- See Note [Absent errors]

  | [UnliftedRep] <- typePrimRep arg_ty
  = Just (Let (NonRec arg unlifted_rhs))

  -- The monomorphic unlifted cases: Bind to some literal, if possible
  -- See Note [Absent errors]
  | Just tc <- tyConAppTyCon_maybe nty
  , Just lit <- absentLiteralOf tc
  = Just (Let (NonRec arg (Lit lit `mkCast` mkSymCo co)))

  | nty `eqType` unboxedUnitTy
  = Just (Let (NonRec arg (Var voidPrimId `mkCast` mkSymCo co)))

  | otherwise
  = WARN( True, text "No absent value for" <+> ppr arg_ty )
    Nothing -- Can happen for 'State#' and things of 'VecRep'
  where
    lifted_arg   = arg `setIdStrictness` botSig `setIdCprInfo` mkCprSig 0 divergeCpr
              -- Note in strictness signature that this is bottoming
              -- (for the sake of the "empty case scrutinee not known to
              -- diverge for sure lint" warning)

    lifted_rhs | isStrictDmd dmd = mkTyApps (Lit (rubbishLit True))  [arg_ty]
               | otherwise       = mkAbsentErrorApp arg_ty msg
    unlifted_rhs = mkTyApps (Lit (rubbishLit False)) [arg_ty]

    arg_ty       = idType arg

    -- Normalise the type to have best chance of finding an absent literal
    -- e.g. (#17852)   data unlifted N = MkN Int#
    --                 f :: N -> a -> a
    --                 f _ x = x
    (co, nty)    = topNormaliseType_maybe (wo_fam_envs opts) arg_ty
                   `orElse` (mkRepReflCo arg_ty, arg_ty)

    msg          = renderWithContext
                     (defaultSDocContext { sdocSuppressUniques = True })
                     (vcat
                       [ text "Arg:" <+> ppr arg
                       , text "Type:" <+> ppr arg_ty
                       , file_msg ])
              -- We need to suppress uniques here because otherwise they'd
              -- end up in the generated code as strings. This is bad for
              -- determinism, because with different uniques the strings
              -- will have different lengths and hence different costs for
              -- the inliner leading to different inlining.
              -- See also Note [Unique Determinism] in GHC.Types.Unique
    file_msg     = case wo_output_file opts of
                     Nothing -> empty
                     Just f  -> text "In output file " <+> quotes (text f)

ww_prefix :: FastString
ww_prefix = fsLit "ww"
