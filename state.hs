module State where
import Graphics.Rendering.OpenGL

data Block = Attributes (X_pos, Y_pos, Z_pos, Click_state, Rotate_state, Degree, Color_state, Color_index)
     deriving (Show)
type X_pos = GLfloat
type Y_pos = GLfloat
type Z_pos = GLfloat
type Click_state = Int
type Rotate_state = Int
type Degree = GLfloat
type Color_state = Int
type Color_index = (GLfloat,GLfloat,GLfloat)

infixl 5 <-*>
(<-*>) :: (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat) -> GLfloat
(a,b,c) <-*> (d,e,f) = (a - d)^2 + (b - e)^2


change_LIST _LIST (Attributes (x, y, z, w, r, d, c, (c_r,c_g,c_b))) = do
                        list <- get _LIST
                        let    threshold = 0.005                               
						in
						_LIST $= (map (\(Attributes (lx, ly, lz, lw, lr, ld, lc, (lc_r,lc_g,lc_b))) -> judge_LIST (Attributes (lx, ly, lz, lw, lr, ld, lc, (lc_r,lc_g,lc_b))) threshold (Attributes (x, y, z, w, r, d, c, (c_r,c_g,c_b)))) $ list)
						
judge_LIST (Attributes (lx, ly, lz, lw, lr, ld, lc, (lc_r,lc_g,lc_b))) threshold (Attributes (x, y, z, w, r, d, c, (c_r,c_g,c_b))) = do
                        let temp = ((lx, ly, lz) <-*> (x, y, z))
                        if ((temp < threshold)) then
									   Attributes (lx, ly, lz, w, r, d, c, (c_r,c_g,c_b)) 
									   --else if ((temp < threshold) && w == 1 && r == 1) then
									       --(lx, ly, lz, w, r, d, c, i)
									   else 
									       Attributes (lx, ly, lz, lw, lr, ld, lc, (lc_r,lc_g,lc_b))
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
                                                                        
