{-# LANGUAGE IncoherentInstances #-}
module Smerdyakov.Internal.Class where

import Data.Proxy (Proxy(Proxy))
import Data.Coerce (coerce)

import Smerdyakov.Internal.FreeIO

-- | An instance of the @Gives@ class says how to generate that target.
--
-- As a general rule, you will only write instances for @Gives@, and never
-- write a function with a @Gives@ constraint. If you need something, say that
-- you 'Needs' it.
--
-- >  make                 | smerdyakov
-- > ----------------------+---------------------------------------------------
-- > target: requirement   | instance (Needs Requirement) => Gives Target where
-- >    action             |    give = action
class Gives a where
    give :: Action a

-- | @make@ gathers, recursively, all the actions needed to fulfill a
-- requirement or target. The 'Action' can then be run with 'interpretIO', or
-- inspected.
make :: forall a proxy r. Gives a => proxy a -> (Needs a => Action r) -> Action r
make _ f = unmake (Proxy :: Proxy a) f

-- We can't have @Gives@ be a superclass of @Needs'@ and still have
-- the representational type role, so we defined the @Needs@ type
-- synonym instead.
class Needs' a

-- | Use @Needs@ to describe a requirement or assumption.
type Needs a = (Needs' a, Gives a)

newtype Wrap b r = Wrap { unWrap :: Needs' b => r }

newtype Make e = Make e
instance Needs' (Make e)

type role Needs' representational

coerceWrap :: Wrap e a -> Wrap (Make e) a
coerceWrap = coerce

unmake :: proxy e -> (Needs' e => a) -> a
unmake _ = unWrap . coerceWrap . Wrap
