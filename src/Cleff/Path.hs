module Cleff.Path where

import Cleff
import Cleff.Error
import Control.Exception
import Data.Kind
import Data.Proxy
import Data.String
import Data.Text (Text, unpack)
import GHC.Exts
import GHC.Generics
import System.Path qualified as Path
import System.Path.Generic qualified as Path.Generic
import System.Path.Part qualified as Path.Part
import System.Path.PartClass qualified as Path.PartClass

type family Unapply (a :: Type -> k) :: Type where
  Unapply (f a) = a

type System = Unapply Path.Path

relFile :: Path.Generic.System os => Text -> Path.Generic.RelFile os
relFile = Path.Generic.relFile . unpack

relDir :: Path.Generic.System os => Text -> Path.Generic.RelDir os
relDir = Path.Generic.relDir . unpack

normalize ::
  forall fd os m.
  ( MonadIO m
  , Path.Generic.System os, Path.PartClass.FileDir fd
  ) =>
  Text ->
  m (Path.Generic.Abs os fd)
normalize =
  liftIO . Path.Generic.dynamicMakeAbsoluteFromCwd .
    Path.Generic.absRel @os @fd . unpack

normalizeFile ::
  forall os m.
  (MonadIO m, Path.Generic.System os) =>
  Text ->
  m (Path.Generic.AbsFile os)
normalizeFile = normalize

normalizeDir ::
  forall os m.
  (MonadIO m, Path.Generic.System os) =>
  Text ->
  m (Path.Generic.AbsDir os)
normalizeDir = normalize
