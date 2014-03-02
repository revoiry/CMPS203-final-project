module Bindings (display,reshape,keyboardMouse,idle) where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Display
import State
import Data.IORef


reshape s@(Size w h) = do 
  viewport $= (Position 0 0, s)

--keyboardAct _ a p (Char ' ') Down _ _ = do
--    a' <-get a
--    a $= -a'

--keyboardAct _ a p (SpecialKey KeyDown) Down _ _ = do
--    (x,y) <-get p
--    p $= (x,y-0.1)
	
keyboardAct _Play_LIST _LIST _ (MouseButton LeftButton) Up _ (Position mou_x mou_y) = do
            list <- get _LIST
            let x = ((fromIntegral mou_x)/250 - 1.0)
                y = (1 - (fromIntegral mou_y)/250)                
		    in 
			change_PlayList _Play_LIST (x, y, 0.0) _LIST;
			let --threshold = 0.005
                x = ((fromIntegral mou_x)/250 - 1.0)
                y = (1 - (fromIntegral mou_y)/250)
			in
			mapM_ (\(lx, ly, lz, w, r, d, c, i) -> judge_block (lx, ly, lz, w, r, d, c, i) 0.005 (x, y, 0.0) _LIST) $ list
			
keyboardAct _ _ _ _ _ _ _ = return ()


-- put new click in the _Play_LIST
change_PlayList _Play_LIST (x, y, z) _LIST = do
                       list2 <- get _LIST;
					   mapM_ (\(l2x, l2y, l2z, w, r, d, c, i) -> judge_w (l2x, l2y, l2z, w, r, d, c, i) (x, y, z) _Play_LIST) $ list2

judge_w (l2x, l2y, l2z, w, r, d, c, i) (x, y, z) _Play_LIST = do
                       play_list <- get _Play_LIST
                       let threshold = 0.005 in
					   if (((x,y,z) <-*> (l2x, l2y, l2z))< threshold && w == 0)then
                                       _Play_LIST $= play_list ++ [(l2x, l2y, l2z)]
									   else
									   return ()
-- change new click state
judge_block (lx, ly, lz, w, r, d, c, i) threshold (x, y, z) _LIST = do
                                    if (((x,y,z) <-*> (lx, ly, lz))< threshold && w == 0) then
									   change_LIST _LIST (lx, ly, lz, 1, 1, d, 1, i)
									   else
									       return ()

keyboardMouse _Play_LIST _LIST pos key state modifiers (Position mou_x mou_y) = do
    keyboardAct _Play_LIST _LIST pos key state modifiers (Position mou_x mou_y)


