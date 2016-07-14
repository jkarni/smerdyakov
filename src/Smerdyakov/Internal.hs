{-# LANGUAGE IncoherentInstances #-}
module Smerdyakov.Internal where

import Data.Proxy (Proxy(Proxy))
import Data.Coerce (coerce)

import Smerdyakov.FreeIO

class Gives a where
    give :: Action a

-- We can't have @Gives@ be a superclass of @Needs'@ and still have
-- the representational type role, so we defined the @Needs@ type
-- synonym instead.
class Needs' a

type Needs a = (Needs' a, Gives a)

newtype Wrap b r = Wrap { unWrap :: Needs' b => r }

newtype Make e = Make e
instance Needs' (Make e)

type role Needs' representational

coerceWrap :: Wrap e a -> Wrap (Make e) a
coerceWrap = coerce

unmake :: proxy e -> (Needs' e => a) -> a
unmake _ = unWrap . coerceWrap . Wrap

make :: forall a r. Gives a => (Needs a => a -> Action r) -> Action r
make f = unmake (Proxy :: Proxy a) (give >>= f)

