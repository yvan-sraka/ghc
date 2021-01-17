{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Haskell.Syntax.Pat where

import Language.Haskell.Syntax.Extension ( XRec )
import Data.Kind

type role Pat nominal
data Pat (i :: Type)
type LPat i = XRec i (Pat i)
