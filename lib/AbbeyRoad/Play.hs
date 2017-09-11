-- |
-- Module      : AbbeyRoad.Play
-- Description : Audio playback with OpenAL and ALUT
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created July 29 2015

-- TODO | - Events (eg. stopped, resumed, finished, etc.)
--        - Streaming (with MVar or [T]Chan (?))

-- SPEC | -
--        -

-- API -------------------------------------------------------------------------

module AbbeyRoad.Play where

-- We'll need these ------------------------------------------------------------

import           Control.Monad

import           Sound.ALUT

import qualified Sound.OpenAL                 as AL
import           Sound.OpenAL.AL.BasicTypes   ()
import           Sound.OpenAL.ALC.Capture

import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM

--- Definitions ----------------------------------------------------------------

-- |
-- TODO | - Rename (?)
data Audio = Audio {
  device  :: _,
  context :: _
} deriving (Show)


-- |
initialise :: IO (Maybe Audio)
initialise = do
  mdevice  <- openDevice Nothing
  mcontext <- createContext device []
  currentContext $= Just context
  return $ Audio <$> mdevice <*> mcontext


loadFrom :: FilePath -> IO (Maybe _)
loadFrom fn = _


-- |
play :: FilePath -> IO ()
play fn = withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ -> do
  Just audio <- initialise
  sound <- createBuffer $ File fn
  [source] <- genObjectNames 1
  queueBuffers source [sound]
  AL.play [source]
  sleep 4
  closeDevice device
  return ()
