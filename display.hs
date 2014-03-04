module Display (display,idle) where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Cube
import State
import Data.IORef

infixl 5 <-*->
(<-*->) :: (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat) -> GLfloat
(a,b,c) <-*-> (d,e,f) = (a - d)^2 + (b - e)^2 + (c - f)^2

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
    mapM_ (\(Attributes (x, y, z, w, r, d, c, (c_r,c_g,c_b))) -> preservingMatrix $ do changeColor (Attributes (x, y, z, w, r, d, c, (c_r,c_g,c_b)));
                                                                                       translate $ Vector3 x y z;
											                                           angle_test (Attributes (x, y, z, w, r, d, c, (c_r,c_g,c_b))) delta _LIST;
											                                           rotateCube (Attributes (x, y, z, w, r, d, c, (c_r,c_g,c_b)));
																					   --cubeFrame 0.1;
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
                          (l1x, l1y, l1z, p1w, p1r, p1d, p1c, (p1c_r,p1c_g,p1c_b)) = search_wr (p1x, p1y, p1z) list num_l
                          (l2x, l2y, l2z, p2w, p2r, p2d, p2c, (p2c_r,p2c_g,p2c_b)) = search_wr (p2x, p2y, p2z) list num_l
					  in						  
					  if ((p1w == 1) && (p1r == 0) && (p2w == 1) && (p2r == 0)) then do
					       --putStrLn $
						       --show(p1r)
					       if (((p1c_r,p1c_g,p1c_b)<-*->(p2c_r,p2c_g,p2c_b))<0.001) then do
						       change_LIST _LIST (Attributes (l1x, l1y, l1z, p1w, p1r, p1d, 2, (p1c_r,p1c_g,p1c_b)));
							   change_LIST _LIST (Attributes (l2x, l2y, l2z, p2w, p2r, p2d, 2, (p2c_r,p2c_g,p2c_b)));
							   _Play_LIST $= drop 2 play_list
							   else do
							       change_LIST _LIST (Attributes (l1x, l1y, l1z, 0, 1, 0.0, 0, (p1c_r,p1c_g,p1c_b)));
							       change_LIST _LIST (Attributes (l2x, l2y, l2z, 0, 1, 0.0, 0, (p2c_r,p2c_g,p2c_b)));
								   _Play_LIST $= drop 2 play_list
					       else
						       return ()

-- search states of elements in _Plat_LIST in _LIST
search_wr (p1x, p1y, p1z) list num_l =
                            if num_l == 0 then
							    (0.0, 0.0, 0.0, 0, 0, 0.0, 0, (0.0,0.0,0.0))
								else do
                                  let threshold = 0.005
                                      Attributes (lx, ly, lz, lw, lr, ld, lc, (lc_r,lc_g,lc_b)) = (list !! (num_l-1))
							      in
							          if ((p1x, p1y, p1z) <-*> (lx, ly, lz)) < threshold then
                                          (lx, ly, lz, lw, lr, ld, lc, (lc_r,lc_g,lc_b))
							             else
							                  search_wr (p1x, p1y, p1z) list (num_l - 1)					   
							   
changeColor (Attributes (x, y, z, w, r, d, c, (c_r,c_g,c_b))) = do
			if c == 1 then
			   color $ Color4 c_r c_g c_b 1.0
			   else if c == 2 then
						color $ Color4 (1.0::GLfloat) 1.0 1.0 0.0
						else
				            color $ Color4 (1-(x+1)/4) (1-(y+1)/4) (1-(z+1)/4) 1.0
				   
rotateCube (Attributes (x, y, z, w, r, d, c, (c_r,c_g,c_b))) = do
            if r == 1 then
                            rotate d $ Vector3 0 (1::GLfloat) 0                           
					else
					     return()

angle_test (Attributes (x, y, z, w, r, d, c, (c_r,c_g,c_b))) delta _LIST = do
            del <- get delta
            if ((r == 1) && (d + del <= 180.0)) then
			       change_LIST _LIST (Attributes (x, y, z, w, r, (d + del), c, (c_r,c_g,c_b)))
				  -- putStrLn $
					    -- show(a+d)
				 else if ((r == 1) && (d < 181.0) && (d + del > 180.0)) then
				       change_LIST _LIST (Attributes (x, y, z, w, 0, 0.0, c, (c_r,c_g,c_b)))
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
