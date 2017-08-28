module InnerEar.Widgets.UserMedia where

import Reflex
import Reflex.Dom
import Reflex.Synth.Types
import Reflex.Synth.Synth
import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
import Reflex.Dom.Contrib.Widgets.Common
import Control.Monad
import Control.Monad.IO.Class(liftIO)
import qualified Data.Map as M

import InnerEar.Types.Score
--import InnerEar.Widgets.Utility



userMediaWidget::MonadWidget t m => Dynamic t Filter -> m()
userMediaWidget filt = do
  src <- bufferInput >>= mapDyn ((flip NodeSource) 2 . BufferNode . File)
  radioWidget <- radioGroup (constDyn "radioWidget") (constDyn $ [(1::Int,"Natural"),(2,"With filter")])
           (WidgetConfig {_widgetConfig_initialValue= Just 2
                         ,_widgetConfig_setValue = never
                         ,_widgetConfig_attributes = constDyn M.empty})
  radioSelection <- mapDyn (maybe 2 id) (_hwidget_value radioWidget) >>= combineDyn (,) filt
  dynSound <- combineDyn (\s (f,i)-> if i==1 then Sound s else FilteredSound s f) src radioSelection
  soundEv <- button "play"
  performSound $ tagDyn dynSound soundEv










--userMediaWidget::MonadWidget t m => Dynamic t Filter -> m ()
--userMediaWidget filt = elClass "div" "userMediaWrapper" $ do

--  source <- mediaElement "userSource" 
--  radioWidget <- radioGroup (constDyn "radioWidget") (constDyn $ [(1::Int,"Natural"),(2,"With filter")])
--           (WidgetConfig {_widgetConfig_initialValue= Just 2
--                         ,_widgetConfig_setValue = never
--                         ,_widgetConfig_attributes = constDyn M.empty})
--  radioSelection <- mapDyn (maybe 2 id) (_hwidget_value radioWidget)
--  dynEffect <- combineDyn (\f i-> if i==1 then GainNode 1 else  FilterNode f) filt radioSelection

--  dynEffect'<- mapDyn createGraph dynEffect   -- dyn m (IO WebAudioGraph)
--  dynEffect'' <- performEvent $ fmap (join . liftIO) $ updated dynEffect'

--  sourceGraph <- liftIO (createGraph source)



--  dest <- liftIO (createNode Destination)

----updatableSound::MonadWidget t m => Dynamic t WebAudioGraph -> Dynamic t WebAudioGraph -> m (Dynamic t  WebAudioGraph)


--  dynEffect''' <- holdDyn (createGraph $ GainNode 1) dynEffect''
--  dynEffect''''<- performEvent  $ fmap (join . liftIO) $ updated dynEffect'''
--  s <- updatableSound (constDyn sourceGraph) dynEffect''''
  
--  performEvent $ fmap (join . updatableSound (constDyn sourceGraph) . liftIO) $ updated dynEffect'''

--  s'' <- updatableSound s (constDyn dest)

--  --mapDyn (createGraph . WebAudioGraph'' sourceGraph) s'
--  --mapDyn show dynSound >>= dynText
--  --playButton <- button "play"
--  --performSound $ tagDyn dynSound playButton
--  return()







--userMediaWidget::MonadWidget t m => Dynamic t Filter -> m ()
--userMediaWidget filt = elClass "div" "userMediaWrapper" $ do

--  source <- mediaElement "userSource"	
--  radioWidget <- radioGroup (constDyn "radioWidget") (constDyn $ [(1::Int,"Natural"),(2,"With filter")])
--           (WidgetConfig {_widgetConfig_initialValue= Just 2
--                         ,_widgetConfig_setValue = never
--                         ,_widgetConfig_attributes = constDyn M.empty})
--  radioSelection <- mapDyn (maybe 2 id) (_hwidget_value radioWidget)
--  dynSound <- combineDyn (\f i-> if i==1 then Sound source else FilteredSound source f) filt radioSelection
--  --connectGraphOnEv $ leftmost [updated dynSound]

--  sourceGraph <- liftIO (createGraph source)

--  filterGraph <- mapDyn createGraph filt

--  plain<-button "normal"
--  filtered <- button "filtered"
--  effect <- holdDyn (GainNode 1) $ leftmost [attachDynWith (\x _->Filter x) filtered, GainNode 1 <$ plain]

--  effect' <- performEvent $ fmap (liftIO . createNode) $ updated effect
--  dest <- liftIO (createNode Destination)
--  liftM (fmap (createGraph (WebAudioGraph'' (WebAudioGraph'' sourceGraph $ WebAudioGraph effect') (WebAudioGraph dest)))) effect'

--  fmap 

--  e <- performEvent $ fmap liftIO $ fmap (\(a,b)->do
--    disconnect a b
--    a' <- createNode a
--    b' <- createNode b
--    return (WebAudioGraph' a' $ WebAudioGraph b')
--    ) $ updated effect
--  performEvent $ fmap (liftIO . connectGraph) e

--  s <- updatableSound effect (constDyn Destination) filterGraph 
--  mapDyn (createGraph . WebAudioGraph'' sourceGraph) s
--  --mapDyn show dynSound >>= dynText
--  --playButton <- button "play"
--  --performSound $ tagDyn dynSound playButton
--  return()





--  -- A dynamic label for Score with CSS style
--dynScoreLabel :: MonadWidget t m => Dynamic t String -> Dynamic t (Score) -> m ()
--dynScoreLabel cssClass score = do
--  score' <- mapDyn ((* 100) . (\x ->  ((fromIntegral (questionsAsked x) :: Float) - (fromIntegral (falseNegatives x) :: Float)) / (fromIntegral (questionsAsked x) :: Float))) score   --m (Dynamic t Float)
--  let r = round 9.0
--  score''  <- mapDyn ((++ "%"). show) score' -- m (Dynamic t String)
--  cssClass' <- mapDyn (M.singleton "class") cssClass  -- m (Dynamic t String)
--  elDynAttr "div" cssClass' $ do
--    dynText  score'' -- m ()
--    return ()


 
