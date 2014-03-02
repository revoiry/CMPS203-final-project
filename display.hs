module Display (display,idle) where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Cube
import State
import Data.IORef

display _Play_LIST _LIST position delta = do 
  list <- get _LIST
  clear [ColorBuffer,DepthBuffer]
  loadIdentity
  (pos_x,pos_y) <- get position
  --translate $ Vector3 x y 0
  translate $ (Vector3 pos_x pos_y (0::GLfloat))  
  --loadIdentity
  --translate (Vector3 0 0 (-2::GLfloat))
  --currentColor $= Color4 1 0 0 1
  preservingMatrix $ do
 --   rotate a $ Vector3 0 0 (1::GLfloat)
    scale 1 1 (1::GLfloat)
    mapM_ (\(x, y, z, w, r, d, c, i) -> preservingMatrix $ do changeColor (x, y, z, w, r, d, c, i);
                                                              translate $ Vector3 x y z;
											                  angle_test (x, y, z, w, r, d, c, i) delta _LIST;
											                  rotateCube (x, y, z, w, r, d, c, i);
                                                              cube (0.08::GLfloat)) $ list
  swapBuffers
  play _Play_LIST _LIST

-- detect whether the first two click block have the same color And delete the first two elements in _Play_LIST
play _Play_LIST _LIST = do
            play_list <- get _Play_LIST
            list <- get _LIST
            let num_p = (length play_list)
                num_l = length list
			in
			if num_p < 2 then
			     return ()
				 else
                      let (p1x, p1y, p1z) = (play_list !! 0)
                          (p2x, p2y, p2z) = (play_list !! 1)
                          (l1x, l1y, l1z, p1w, p1r, p1d, p1c, p1i) = search_wr (p1x, p1y, p1z) list num_l
                          (l2x, l2y, l2z, p2w, p2r, p2d, p2c, p2i) = search_wr (p2x, p2y, p2z) list num_l
					  in						  
					  if ((p1w == 1) && (p1r == 0) && (p2w == 1) && (p2r == 0)) then do
					       putStrLn $
						       show(p1r)
					       if p1i == p2i then do
						       change_LIST _LIST (l1x, l1y, l1z, p1w, p1r, p1d, 2, p1i);
							   change_LIST _LIST (l2x, l2y, l2z, p2w, p2r, p2d, 2, p2i);
							   _Play_LIST $= drop 2 play_list
							   else do
							       change_LIST _LIST (l1x, l1y, l1z, 0, 1, 0.0, 0, p1i);
							       change_LIST _LIST (l2x, l2y, l2z, 0, 1, 0.0, 0, p2i);
								   _Play_LIST $= drop 2 play_list
					       else
						       putStrLn $
						               show(p1r)

-- search states of elements in _Plat_LIST in _LIST
search_wr (p1x, p1y, p1z) list num_l =
                            if num_l == 0 then
							    (0.0, 0.0, 0.0, 0, 0, 0.0, 0, 0)
								else do
                                  let threshold = 0.005
                                      (lx, ly, lz, lw, lr, ld, lc, li) = (list !! (num_l-1))
							      in
							          if ((p1x, p1y, p1z) <-*> (lx, ly, lz)) < threshold then
                                         (lx, ly, lz, lw, lr, ld, lc, li)
							             else
							                  search_wr (p1x, p1y, p1z) list (num_l - 1)					   
							   
changeColor (x, y, z, w, r, d, c, i) = do
			if c == 1 then
			   color $ Color4 (1.0::GLfloat) 0.5 0.5 1.0
			   else if c == 2 then
						color $ Color4 (1.0::GLfloat) 1.0 1.0 0.0
						else
				            color $ Color4 ((x+1)/2.0) ((y+1)/2.0) ((z+1)/2.0) 1.0
				   
rotateCube (x, y, z, w, r, d, c, i) = do
            if r == 1 then
                            rotate d $ Vector3 0 (1::GLfloat) 0                           
					else
					     return()

angle_test (x, y, z, w, r, d, c, i) delta _LIST = do
            del <- get delta
            if ((r == 1) && (d + del <= 180.0)) then
			       change_LIST _LIST (x, y, z, w, r, (d + del), c, i)
				  -- putStrLn $
					    -- show(a+d)
				 else if ((r == 1) && (d + del < 182.0) && (d + del > 180.0)) then
				       change_LIST _LIST (x, y, z, w, 0, 0.0, c, i)
					   else
					     return ()

				  
idle = do
  postRedisplay Nothing
  
  
--points :: [(GLfloat,GLfloat,GLfloat)]
--points = map (\k -> (sin(2*pi*k/12),cos(2*pi*k/12),0.0))  [1..12]

--display = do 
--  clear [ColorBuffer]
--  do mapM (\(x,y,z) -> preservingMatrix $ do color $Color3 x y z
--                                            translate $ Vector3 x y z
--                                             cube (0.1::GLfloat)) points
--  flush
