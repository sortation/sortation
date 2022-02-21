module Prelude
  ( module BasePrelude
  , module Control.Applicative
  , module Control.Concurrent.STM
  , module Control.Concurrent.STM.TVar
  , module Control.Exception
  , module Control.Monad
  , module Control.Monad.Catch
  , module Control.Monad.IO.Class
  , module Control.Monad.Loops
  , module Control.Natural
  , module Data.Bool
  , module Data.ByteString
  , module Data.Foldable
  , module Data.Functor
  , module Data.Functor.Identity
  , module Data.Kind
  , module Data.Map
  , module Data.Set
  , module Data.Text
  , module Data.Text.IO
  , module Data.Vector
  , module Data.Word
  , module Effectful
  , module Effectful.Reader.Static
  , module GHC.Prim
  , module GHC.TypeLits
  , module Optics
  , module Text.Pretty.Simple
  , module Text.Read
  , Generic
  , ifThenElse
  , getField
  , setField
  , IsLabel(..)
  , IsList(fromListN)
  , IsString(..)
  )
  where

import BasePrelude hiding
  ( head, tail
  , putStr, putStrLn
  , appendFile, writeFile, readFile
  , getLine, getContents
  , interact
  )

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception (Exception)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Natural (type (~>))
import Data.Bool
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Functor
import Data.Functor.Identity
import Data.Kind
import Data.Map (Map)
import Data.Set (Set)
import Data.String
import Data.Text (Text, pack, unpack)
import Data.Text.IO
import Data.Vector (Vector)
import Data.Word
import Effectful
import Effectful.Reader.Static
import GHC.Exts
import GHC.Generics
import GHC.OverloadedLabels
import GHC.Prim
import GHC.TypeLits hiding (ErrorMessage(..))
import Optics hiding ((:>))
import Text.Pretty.Simple
import Text.Read (readMaybe)

ifThenElse :: Bool -> a -> a -> a
ifThenElse = \cond true false -> bool false true cond

getField ::
  forall x r a o is.
  (Is o A_Getter, IsLabel x (Optic' o is r a)) =>
  r -> a
getField = view (fromLabel @x @(Optic' o is r a))

setField ::
  forall x r a o is.
  (Is o A_Setter, IsLabel x (Optic' o is r a)) =>
  r -> a -> r
setField = flip (set (fromLabel @x @(Optic' o is r a)))
