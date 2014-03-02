module State where
import Graphics.Rendering.OpenGL

infixl 5 <-*>
(<-*>) :: (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat) -> GLfloat
(a,b,c) <-*> (d,e,f) = (a - d)^2 + (b - e)^2


change_LIST _LIST (x, y, z, w, r, d, c, i) = do
                        list <- get _LIST
                        let    threshold = 0.005                               
						in
						_LIST $= (map (\(lx, ly, lz, lw, lr, ld, lc, li) -> judge_LIST (lx, ly, lz, lw, lr, ld, lc, li) threshold (x, y, z, w, r, d, c, i)) $ list)
						
judge_LIST (lx, ly, lz, lw, lr, ld, lc, li) threshold (x, y, z, w, r, d, c, i) = do
                        let temp = ((lx, ly, lz) <-*> (x, y, z))
                        if ((temp < threshold)) then
									   (lx, ly, lz, w, r, d, c, i) 
									   --else if ((temp < threshold) && w == 1 && r == 1) then
									       --(lx, ly, lz, w, r, d, c, i)
									   else 
									       (lx, ly, lz, lw, lr, ld, lc, li)
--module Display (display) where

--import Graphics.Rendering.OpenGL
--import Graphics.UI.GLUT

--import Cube
--import Points

--display = do                                                            
--  clear [ColorBuffer]                                                   
--  do mapM (\(x,y,z) -> preservingMatrix $ do color $Color3 ((x+1.0)/2.0 ((y+1.0)/2.0) ((z+1.0)/2.0)        
--                                             translate $ Vector3 x y z  
--                                             cube (0.1::GLfloat)) $ points 7
--  flush                                                                 
                                                                        
