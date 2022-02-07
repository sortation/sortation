module Cleff.Optics where

import Cleff
import Cleff.Reader
import Optics

peruse ::
  forall r k a is es.
  (Reader r :> es, Is k A_Getter) =>
  Optic' k is r a ->
  Eff es a
peruse l = view l <$> ask
