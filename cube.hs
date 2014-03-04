module Cube where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

--cubeFrame w = do
  --renderPrimitive Lines $ do
           --  mapM_ Vertex3 [( w,-w, w), ( w, w, w),  ( w, w, w), (-w, w, w),
                           -- (-w, w, w), (-w,-w, w),  (-w,-w, w), ( w,-w, w),
                         --   ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w),
                         --   (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
                        --    ( w,-w,-w), ( w, w,-w),  ( w, w,-w), (-w, w,-w),
                       --     (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w)]
	
cube w = do 
  renderPrimitive Quads $ do
    --vertex $ Vertex3 w w w
    --vertex $ Vertex3 w w (-w)
    --vertex $ Vertex3 w (-w) (-w)
    --vertex $ Vertex3 w (-w) w

    --vertex $ Vertex3 w w w
    --vertex $ Vertex3 w w (-w)
    --vertex $ Vertex3 (-w) w (-w)
    --vertex $ Vertex3 (-w) w w
	
    vertex $ Vertex3 w w 0.0
    vertex $ Vertex3 w (-w) 0.0
    vertex $ Vertex3 (-w) (-w) 0.0
    vertex $ Vertex3 (-w) w 0.0
    --vertex $ Vertex3 w w w
    --vertex $ Vertex3 w (-w) w
    --vertex $ Vertex3 (-w) (-w) w
    --vertex $ Vertex3 (-w) w w

  --  currentColor $= Color4 1 0 0 1
    
    --vertex $ Vertex3 (-w) w w
    --vertex $ Vertex3 (-w) w (-w)
    --vertex $ Vertex3 (-w) (-w) (-w)
    --vertex $ Vertex3 (-w) (-w) w

    --vertex $ Vertex3 w (-w) w
    --vertex $ Vertex3 w (-w) (-w)
    --vertex $ Vertex3 (-w) (-w) (-w)
    --vertex $ Vertex3 (-w) (-w) w

    --vertex $ Vertex3 w w (-w)
    --vertex $ Vertex3 w (-w) (-w)
    --vertex $ Vertex3 (-w) (-w) (-w)
    --vertex $ Vertex3 (-w) w (-w)
