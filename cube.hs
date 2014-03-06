module Cube where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import State
--(-w)->(-w/10)(w)->(w/10)
cubeFrame w = do
  renderPrimitive Lines $ do
              vertex $ Vertex3 w (-w) (w/10)
              vertex $ Vertex3 w  w (w/10)
              
              vertex $ Vertex3 w w (w/10)
              vertex $ Vertex3 (-w) w (w/10)

              vertex $ Vertex3 (-w) w (w/10)
              vertex $ Vertex3 (-w) (-w) (w/10)

              vertex $ Vertex3 (-w) (-w) (w/10)
              vertex $ Vertex3 w (-w) (w/10)

              vertex $ Vertex3 w (-w) (w/10)
              vertex $ Vertex3 w (-w) (-w/10)

              vertex $ Vertex3 w w (w/10)
              vertex $ Vertex3 w w (-w/10)

              vertex $ Vertex3 (-w) w (w/10)
              vertex $ Vertex3 (-w) w (-w/10)

              vertex $ Vertex3 (-w) (-w) (w/10)
              vertex $ Vertex3 (-w) (-w) (-w/10)

              vertex $ Vertex3 w (-w) (-w/10)
              vertex $ Vertex3 w w (-w/10)

              vertex $ Vertex3 w w (-w/10)
              vertex $ Vertex3 (-w) w (-w/10)

              vertex $ Vertex3 (-w) w (-w/10)
              vertex $ Vertex3 (-w) (-w) (-w/10)

              vertex $ Vertex3 (-w) (-w) (-w/10)
              vertex $ Vertex3 w (-w) (-w/10)

	
cube w (Attributes (x, y, z, w_w, r, d, c, (c_r,c_g,c_b))) = do 
  --preservingMatrix $ do  
    renderPrimitive Quads $ do
       --color $ Color4 (0.5::GLfloat) 0.5 0.5 1.0
       --color $ Color4 (0.5::GLfloat) 0.5 0.5 1.0
       vertex $ Vertex3 w w (w/10)
       vertex $ Vertex3 w w (-w/10)
       vertex $ Vertex3 w (-w) (-w/10)
       vertex $ Vertex3 w (-w) (w/10)

       vertex $ Vertex3 w w (w/10)
       vertex $ Vertex3 w w (-w/10)
       vertex $ Vertex3 (-w) w (-w/10)
       vertex $ Vertex3 (-w) w (w/10)

       --color $ Color4 ((1-(x+1)/4)::GLfloat) (1-(y+1)/4) (1-(z+1)/4) 1.0
       vertex $ Vertex3 w w (w/10)
       vertex $ Vertex3 w (-w) (w/10)
       vertex $ Vertex3 (-w) (-w) (w/10)
       vertex $ Vertex3 (-w) w (w/10)

       --color $ Color4 (0.5::GLfloat) 0.5 0.5 1
       vertex $ Vertex3 (-w) w (w/10)
       vertex $ Vertex3 (-w) w (-w/10)
       vertex $ Vertex3 (-w) (-w) (-w/10)
       vertex $ Vertex3 (-w) (-w) (w/10)

       vertex $ Vertex3 w (-w) (w/10)
       vertex $ Vertex3 w (-w) (-w/10)
       vertex $ Vertex3 (-w) (-w) (-w/10)
       vertex $ Vertex3 (-w) (-w) (w/10)

       --color $ Color4 (c_r::GLfloat) c_g c_b 1
       vertex $ Vertex3 w w (-w/10)
       vertex $ Vertex3 w (-w) (-w/10)
       vertex $ Vertex3 (-w) (-w) (-w/10)
       vertex $ Vertex3 (-w) w (-w/10)
    flush
