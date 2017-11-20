-- |
-- Module      : AbbeyRoad.Play
-- Description : Audio playback with OpenAL and ALUT
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- TODO | - Events (eg. stopped, resumed, finished, etc.)
--        - Streaming (with MVar or [T]Chan (?))
--        - Figure out what exceptions ALUT and OpenAL might throw our way
--        - Dealing with the state machine, reloading and cleaning up
--        - GHCi support
--        - Nomenclature (come up with a coherent and memorable set of idioms)

-- API -------------------------------------------------------------------------

module AbbeyRoad.Play where

-- We'll need these ------------------------------------------------------------

import           Control.Concurrent
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Loops
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Either

import           Sound.ALUT                   (get, ($=))
import qualified Sound.ALUT                   as ALUT
import qualified Sound.OpenAL                 as AL
import           Sound.OpenAL.AL.BasicTypes   ()
import           Sound.OpenAL.ALC.Capture

import           Lens.Micro

import           Data.Monoid
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM

--- Definitions ----------------------------------------------------------------

-- |
-- TODO | - Rename (?)
data Conductor = Conductor {
  fDevice  :: AL.Device,
  fContext :: AL.Context,
  fSource  :: AL.Source,
  fSheets  :: [AL.Buffer]
} deriving (Show)

(~>) = (^.)

device :: SimpleGetter Conductor AL.Device
device = to fDevice

context :: SimpleGetter Conductor AL.Context
context = to fContext

source :: SimpleGetter Conductor AL.Source
source = to fSource

-- | Promote a 'Maybe' to an 'Either'
-- TODO | - Factor out
explain :: e -> Maybe a -> Either e a
explain e = maybe (Left e) Right


-- |
isPlaying :: MonadIO m => Conductor -> m Bool
isPlaying conductor = (== AL.Playing) <$> (get (AL.sourceState $ conductor~>source))


-- |
initialise :: EitherT String IO Conductor
initialise = do
  device   <- EitherT $ explain "Could not open device"    <$> AL.openDevice Nothing
  context  <- EitherT $ explain "Could not create context" <$> AL.createContext device []
  AL.currentContext $= Just context
  [source] <- lift $ AL.genObjectNames 1
  right $ Conductor device context source []


-- |
-- TODO | - What if the file doesn't exist
loadFrom :: FilePath -> EitherT String IO AL.Buffer
loadFrom fn = catchAny
                (lift . ALUT.createBuffer $ ALUT.File fn)
                (\e -> left $ "Could not create buffer for file '" <> fn <> "': " <> show e)


-- |
-- TODO | -
--        -
-- x :: MonadIO m => String -> [String] -> ((String -> [String] -> (m a)) -> m a)
-- x = runALUTUsingCurrentContext
play :: Conductor -> FilePath -> EitherT String IO ()
play conductor fn = do
  sound <- loadFrom fn
  AL.queueBuffers (conductor~>source) [sound]
  AL.play [conductor~>source]
  void $ whileM (isPlaying conductor) (lift . threadDelay . floor $ 0.05 * 10^6)


-- |
-- TOOD | - Rename
run :: IO (Either String ())
run = ALUT.withProgNameAndArgs ALUT.runALUTUsingCurrentContext $ \_ _ -> runEitherT $ do
  conductor <- initialise
  play conductor "assets/doorbell.wav"
  cleanup conductor


-- |
cleanup :: MonadIO m => Conductor -> m ()
cleanup conductor = void $ do
  AL.currentContext $= Nothing
  AL.destroyContext $ conductor~>context
  AL.closeDevice    $ conductor~>device  -- TODO: This returns a Bool, so use the value somehow (?)


-- |
pass :: Applicative f => f ()
pass = pure ()
