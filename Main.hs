import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Bindings
import State
import RandomColor
import Data.IORef


_INITIAL_WIDTH :: GLsizei
_INITIAL_WIDTH=500
_INITIAL_HEIGHT::GLsizei
_INITIAL_HEIGHT=500



infixl 5 <+>
Attributes (x, y, z, w, r, d, c, (c_r,c_g,c_b)) <+> Attributes (lx, ly, lz, lw, lr, ld, lc, (lc_r,lc_g,lc_b)) = Attributes (x + lx, y + ly, z + lz, 0, 0, 0.0, 0, (0.0,0.0,0.0))
points' n' = let n = fromIntegral n' in map (\k -> let t = -0.6+(1.2)*k/n in (Attributes (t, 0.0, 0.0, 0, 0, 0.0, 0, (0.0,0.0,0.0))))  [1..n]
points'' a b = let n = fromIntegral a in map (\k -> let t = -0.6+(1.2)*k/n in ((Attributes (0.0, t, 0.0, 0, 0, 0.0, 0, (0.0,0.0,0.0))) <+> ((points' a) !! b)))  [1..n]
points n = (points'' n 0)++(points'' n 1)++(points'' n 2)++(points'' n 3)++(points'' n 4)++(points'' n 5)

main = do
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "Minesweeper_v0.1"
  reshapeCallback $= Just reshape
  --angle <- newIORef (0::GLfloat)
  delta <- newIORef (10.0::GLfloat)
  position <- newIORef (0.0::GLfloat, 0.0)
  _LIST <- newIORef (init_color 36 (points 6)) --initialize the matrix as a list, the element of which is like (x, y, z,w) and the w is the state referring to whether the block is clicked.
  _Play_LIST <- newIORef ([])
  --fullScreen
  windowSize $= Size _INITIAL_WIDTH _INITIAL_HEIGHT
  keyboardMouseCallback $= Just (keyboardMouse _Play_LIST _LIST position)

  displayCallback $= (display _Play_LIST _LIST position delta)
  idleCallback $= Just (idle)
  
  matrixMode $= Projection
  
  loadIdentity
  let near = 0
      far = 40
      right = 1
      top = 1
  frustum (-right) right (-top) top near far
  matrixMode $= Modelview 0
  clearColor $= Color4 1 1 1 1

  mainLoop
