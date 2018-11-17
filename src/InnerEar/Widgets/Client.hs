{-# LANGUAGE RecursiveDo, OverloadedStrings, DeriveDataTypeable #-}
module InnerEar.Widgets.Client where

import Control.Monad
import Data.Maybe
import Control.Monad.IO.Class (liftIO)

import GHCJS.Types
import GHCJS.Marshal.Internal
import GHCJS.Foreign.Callback

import Data.Map(Map, fromList, empty)
import Data.JSString(JSString, pack)

import Reflex
import Reflex.Dom

import Text.JSON
import Text.JSON.Generic

import InnerEar.Types.Response
import InnerEar.Types.Request
import InnerEar.Widgets.Utility
import InnerEar.Widgets.Login
import InnerEar.Widgets.Navigation
import qualified InnerEar.WebSocket as WS
import Sound.MusicW

-- | The clientWidget is the top-level widget in the Inner Ear web client.
-- At first it displays the login widget plus a choice to continue without logging in.
-- Successfully logging in (or choosing to continue without logging in) loads
-- the exercise navigationWidget instead.

performSynth :: MonadWidget t m => Event t (Maybe (Synth ())) -> m ()
performSynth selectedSynth = mdo
  -- Behaviour t (Maybe Synthstance) - the previously instantiated synth
  prevSynthstance <- hold Nothing newSynthstances

  -- Event t (Maybe Synthstance, Maybe (Synth ())) - prev and current
  let synthstancePair = attach prevSynthstance selectedSynth

  -- Event t (Maybe Synthstance)
  newSynthstances <- performEvent $ ffor synthstancePair $ \(prev, curr) -> liftIO $ do
    -- Stop the old one if it existed
    maybe (return ()) stopSynthNow prev
    -- Start the new one if given
    maybe (return Nothing) (\s -> instantiateSynth s >>= startSynthNow >>= return . Just) curr

  return ()

clientWidget :: MonadWidget t m => Dynamic t (Map String AudioBuffer) -> m ()
clientWidget sysResources = elClass "div" "innerEar" $ mdo
  (wsRcvd,wsStatus) <- WS.reflexWebSocket wsSend
  elClass "div" "title" $ text "Inner Ear"
  loginVisible <- holdDyn True $ leftmost [True <$ loggedIn, True <$ noLogin, True <$ loggedOut]
  navVisible <- holdDyn False $ leftmost [True <$ loggedIn, True <$ noLogin, False <$ loggedOut]
  bypassVisible <- holdDyn True $ leftmost [False <$ loggedIn, False <$ noLogin, True <$ loggedOut]
  (loginRequests,currentRole) <- elClass "div" "login" $ visibleWhen loginVisible $ loginWidget wsRcvd
  let loggedIn = ffilter isJust $ updated currentRole
  let loggedOut = ffilter isNothing $ updated currentRole
  noLogin <- elClass "div" "loginBypass" $ visibleWhen bypassVisible $ button "Continue without logging in"
  (navRequests,sounds) <- visibleWhen navVisible $ navigationWidget sysResources wsRcvd currentRole
  let wsSend = leftmost [loginRequests,navRequests]
  performSynth sounds
  return ()

loadGlobalResources :: MonadWidget t m => m ( Event t  (Map String AudioBuffer))
loadGlobalResources = do
  let sources = ["pinknoise.wav", "whitenoise.wav"]
  pb <- getPostBuild
  bufferEv <- loadResources $ fmap (const sources) pb -- Ev (Map String Buffer)
  let r = fmap (fmap js_getAudioBuffer) bufferEv -- Ev (Map String (IO AudioBuffer))
  let r' = fmap (liftIO . sequence) r  -- ev (io (Map String AudioBuffer))
  performEvent r'

loadResources:: MonadWidget t m => Event t [String] -> m (Event t (Map String Buffer))
loadResources ev = do
  bufferEv <- performEvent $ ffor ev $ \fileNames -> liftIO $ do                     -- Ev t [(String,Buffer)]
    ctx <- globalAudioContext -- really just get global ac
    forM fileNames $ \s -> do
      buf <- js_createBufferFromURL (pack s) ctx
      return (s,buf)
  stateChangeEv <- performEventAsync $ ffor bufferEv $ \buffers evTrigger -> liftIO $ do  -- Event t [Buffer]
    -- cbs <- forM buffers $ \(s,buf) -> asyncCallback1 $ \buf -> evTrigger (Buffer buf)  -- IO [cb]
    cb <- asyncCallback1 $ \bufs -> do
      maybeBufs <- fromJSValListOf bufs -- IO (Maybe [Buffer])
      evTrigger (maybe [] (fmap Buffer) maybeBufs)
    func (fmap snd buffers) cb
    releaseCallback cb
  statusEv <- performEvent $ ffor stateChangeEv $ \buffers -> do   -- Event t [Maybe BufferStatus]
    liftIO $ sequence $ fmap bufferStatus buffers -- IO [(Maybe BufferStatus)] then lifted
  let areLoaded = fmap (and . fmap (maybe False (isBufferLoaded))) statusEv
  dynBuffers <- holdDyn Data.Map.empty $ fmap fromList bufferEv
  return $ tagDyn dynBuffers $ ffilter id areLoaded
  where
    func:: [Buffer] -> Callback (JSVal -> IO ()) -> IO ()
    func b cb  = do
      listJSVal <- toJSValListOf b
      js_startLoadingAndDecodingMultiple listJSVal cb
