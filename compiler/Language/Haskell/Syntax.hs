{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section{Haskell abstract syntax definition}

This module glues together the pieces of the Haskell abstract syntax,
which is declared in the various \tr{Hs*} modules.  This module,
therefore, is almost nothing but re-exporting.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-} -- For deriving instance Data

module Language.Haskell.Syntax (
        module Language.Haskell.Syntax.Binds,
        module Language.Haskell.Syntax.Decls,
        module Language.Haskell.Syntax.Expr,
        module Language.Haskell.Syntax.Lit,
        module Language.Haskell.Syntax.Pat,
        module Language.Haskell.Syntax.Type,
        module Language.Haskell.Syntax.Extension,
) where

import Language.Haskell.Syntax.Decls
import Language.Haskell.Syntax.Binds
import Language.Haskell.Syntax.Expr
import Language.Haskell.Syntax.Lit
import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Pat
import Language.Haskell.Syntax.Type

{-
Note [Language.Haskell.Syntax.* Hierarchy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Why are these modules not 'GHC.Hs.*', or some other 'GHC.*'? The answer
is that they are to be separated from GHC and put into another package,
in accordance with the final goals of Trees that Grow. (See Note [Trees
that grow] in 'Language.Haskell.Syntax.Extension'.)

We cannot separate them yet, but by giving them names like so, we hope
to remind others that the goal is to factor them out, and therefore
dependencies on the rest of GHC should never be added, only removed.

For more details, see
https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow
-}


-- TODO Add TTG parameter to 'HsModule' and move here.
