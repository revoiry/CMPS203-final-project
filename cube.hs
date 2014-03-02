module Cube where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
cube w = do 
  renderPrimitive Quads $ do
 --   vertex $ Vertex3 w w w
 --   vertex $ Vertex3 w w (-w)
  --  vertex $ Vertex3 w (-w) (-w)
 --   vertex $ Vertex3 w (-w) w

  --  vertex $ Vertex3 w w w
  --  vertex $ Vertex3 w w (-w)
  --  vertex $ Vertex3 (-w) w (-w)
  --  vertex $ Vertex3 (-w) w w

    vertex $ Vertex3 w w 0.0
    vertex $ Vertex3 w (-w) 0.0
    vertex $ Vertex3 (-w) (-w) 0.0
    vertex $ Vertex3 (-w) w 0.0

  --  currentColor $= Color4 1 0 0 1
    
  --  vertex $ Vertex3 (-w) w w
  --  vertex $ Vertex3 (-w) w (-w)
  --  vertex $ Vertex3 (-w) (-w) (-w)
  --  vertex $ Vertex3 (-w) (-w) w

   -- vertex $ Vertex3 w (-w) w
   -- vertex $ Vertex3 w (-w) (-w)
  --  vertex $ Vertex3 (-w) (-w) (-w)
  --  vertex $ Vertex3 (-w) (-w) w

  --  vertex $ Vertex3 w w (-w)
   -- vertex $ Vertex3 w (-w) (-w)
  --  vertex $ Vertex3 (-w) (-w) (-w)
  --  vertex $ Vertex3 (-w) w (-w)
