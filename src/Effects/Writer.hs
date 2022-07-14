{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Effects.Writer where
import Prog

-- ** (Section 5.5) Writer effect 
data Writer w a where
  Tell :: w -> Writer w ()

tell :: Member (Writer w) ts => w -> Prog ts ()
tell w = Op (inj $ Tell w) Val

handleWriter :: forall w ts a . Monoid w => Prog (Writer w ': ts) a -> Prog ts (a, w)
handleWriter = loop mempty where
  loop ::  w -> Prog (Writer w ': ts) a -> Prog ts (a, w)
  loop w (Val x) = return (x, w)
  loop w (Op u k) = case discharge u of
    Right (Tell w') -> loop (w `mappend` w') (k ())
    Left u'         -> Op u' (loop w . k)
