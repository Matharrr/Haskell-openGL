{-
   Brick.hs (adapted from ogl2brick.c which is (c) 3Dlabs Inc. Ltd.)
   Copyright (c) Sven Panne 2006 <svenpanne@gmail.com>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE
-}

import Prelude hiding ( sum )
import Control.Applicative
import Control.Exception ( IOException, catch )
import Control.Monad ( when, unless )
import qualified Data.ByteString as B
import Data.Foldable ( Foldable, sum )
import Data.IORef
import System.Exit
import Graphics.UI.GLUT

infixl 6 $+, $-
infixl 7 $*

inertiaThreshold, inertiaFactor :: GLfloat
inertiaThreshold = 1
inertiaFactor = 0.5

scaleFactor, scaleIncrement :: GLfloat
scaleFactor = 0.01
scaleIncrement = 0.5

timerFrequencyMillis :: Timeout
timerFrequencyMillis = 20

clearColors :: [Color4 GLclampf]
clearColors = [
   Color4 0.0 0.0 0.0 1,
   Color4 0.2 0.2 0.3 1,
   Color4 0.7 0.7 0.7 1 ]

models :: [IO ()]
models = [
   drawCube,
   renderObject Solid (Teapot 0.6),
   renderObject Solid (Sphere' 0.6 64 64),
   renderObject Solid (Torus 0.2 0.6 64 64) ]

initialDiff :: Vector3 GLfloat
initialDiff = Vector3 206 16 10

initialInertia :: Vector3 GLfloat
initialInertia = Vector3 (-0.5) 0 0

data State = State {
   diff :: IORef (Vector3 GLfloat),
   lastIncr :: IORef (Vector3 GLfloat),
   inertia :: IORef (Vector3 GLfloat),
   inertiaOld :: IORef (Vector3 GLfloat),
   theScale :: IORef GLfloat,
   lastPosition :: IORef Position,
   shouldRotate :: IORef Bool,
   colorCycle :: IORef [Color4 GLclampf],
   modelCycle :: IORef [IO ()],
   modifiers :: IORef Modifiers
   }

makeState :: IO State
makeState = do
   di <- newIORef initialDiff
   li <- newIORef (pure 0)
   ia <- newIORef initialInertia
   io <- newIORef (pure 0)
   sc <- newIORef (1 - scaleIncrement)  -- Mengurangkan scaleIncrement dari nilai awal
   lp <- newIORef (Position (-1) (-1))
   sr <- newIORef True
   cc <- newIORef (cycle clearColors)
   mc <- newIORef (cycle models)
   mo <- newIORef (Modifiers Up Up Up)
   return $ State {
      diff = di,
      lastIncr = li,
      inertia = ia,
      inertiaOld = io,
      theScale = sc,
      lastPosition = lp,
      shouldRotate = sr,
      colorCycle = cc,
      modelCycle = mc,
      modifiers = mo
   }


-- Our tiny vector math library...
($+), ($-), ($*) :: (Applicative t, Num a) => t a -> t a -> t a
($+) = liftA2 (+)
($-) = liftA2 (-)
($*) = liftA2 (*)

step :: (Applicative t, Num a, Ord a) => t a -> t a -> t a
step = liftA2 (\e x -> if x < e then 0 else 1)

dot :: (Applicative t, Foldable t, Num a) => t a -> t a -> a
dot v1 v2 = sum (v1 $* v2)

drawFace :: Normal3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat
         -> Vertex3 GLfloat -> Vertex3 GLfloat -> IO ()
drawFace p q r s t = do
   let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
   normal p
   texCoord2f (TexCoord2 1 1)
   vertex q
   texCoord2f (TexCoord2 0 1)
   vertex r
   texCoord2f (TexCoord2 0 0)
   vertex s
   texCoord2f (TexCoord2 1 0)
   vertex t

drawCube :: IO ()
drawCube = do
  let
      v0 = Vertex3 (-2.07) (-1.898097) (0.72)
      v1 = Vertex3 (-2.07) (3.561903) (0.72)
      v2 = Vertex3 (-2.07) (-1.898097) (-0.72)
      v3 = Vertex3 (-2.07) (3.561903) (-0.72)
      v4 = Vertex3 (2.07) (-1.898097) (0.72)
      v5 = Vertex3 (2.07) (3.561903) (0.72)
      v6 = Vertex3 (2.07) (-1.898097) (-0.72)
      v7 = Vertex3 (2.07) (3.561903) (-0.72)
      v8 = Vertex3 (0.0) (3.530698) (-1.0)
      v9 = Vertex3 (0.19509) (3.530698) (-0.980785)
      v10 = Vertex3 (0.382683) (3.530698) (-0.92388)
      v11 = Vertex3 (0.55557) (3.530698) (-0.83147)
      v12 = Vertex3 (0.707107) (3.530698) (-0.707107)
      v13 = Vertex3 (0.83147) (3.530698) (-0.55557)
      v14 = Vertex3 (0.92388) (3.530698) (-0.382683)
      v15 = Vertex3 (0.980785) (3.530698) (-0.19509)
      v16 = Vertex3 (1.0) (3.530698) (0.0)
      v17 = Vertex3 (0.980785) (3.530698) (0.19509)
      v18 = Vertex3 (0.92388) (3.530698) (0.382683)
      v19 = Vertex3 (0.83147) (3.530698) (0.55557)
      v20 = Vertex3 (0.707107) (3.530698) (0.707107)
      v21 = Vertex3 (0.55557) (3.530698) (0.83147)
      v22 = Vertex3 (0.382683) (3.530698) (0.92388)
      v23 = Vertex3 (0.19509) (3.530698) (0.980785)
      v24 = Vertex3 (0.0) (3.530698) (1.0)
      v25 = Vertex3 (-0.19509) (3.530698) (0.980785)
      v26 = Vertex3 (-0.382683) (3.530698) (0.92388)
      v27 = Vertex3 (-0.55557) (3.530698) (0.83147)
      v28 = Vertex3 (-0.707107) (3.530698) (0.707107)
      v29 = Vertex3 (-0.83147) (3.530698) (0.55557)
      v30 = Vertex3 (-0.92388) (3.530698) (0.382683)
      v31 = Vertex3 (-0.980785) (3.530698) (0.19509)
      v32 = Vertex3 (-1.0) (3.530698) (0.0)
      v33 = Vertex3 (-0.980785) (3.530698) (-0.19509)
      v34 = Vertex3 (-0.92388) (3.530698) (-0.382683)
      v35 = Vertex3 (-0.83147) (3.530698) (-0.55557)
      v36 = Vertex3 (-0.707107) (3.530698) (-0.707107)
      v37 = Vertex3 (-0.55557) (3.530698) (-0.83147)
      v38 = Vertex3 (-0.382683) (3.530698) (-0.92388)
      v39 = Vertex3 (-0.19509) (3.530698) (-0.980785)
      v40 = Vertex3 (0.0) (5.530698) (0.0)
      n0 = Normal3 (-1.0) (-0.0) (-0.0)
      n1 = Normal3 (-0.0) (-0.0) (-1.0)
      n2 = Normal3 (1.0) (-0.0) (-0.0)
      n3 = Normal3 (-0.0) (-0.0) (1.0)
      n4 = Normal3 (-0.0) (-1.0) (-0.0)
      n5 = Normal3 (-0.0) (1.0) (-0.0)
      n6 = Normal3 (0.0878) (0.4455) (-0.891)
      n7 = Normal3 (0.2599) (0.4455) (-0.8567)
      n8 = Normal3 (0.422) (0.4455) (-0.7896)
      n9 = Normal3 (0.568) (0.4455) (-0.6921)
      n10 = Normal3 (0.6921) (0.4455) (-0.568)
      n11 = Normal3 (0.7896) (0.4455) (-0.422)
      n12 = Normal3 (0.8567) (0.4455) (-0.2599)
      n13 = Normal3 (0.891) (0.4455) (-0.0878)
      n14 = Normal3 (0.891) (0.4455) (0.0878)
      n15 = Normal3 (0.8567) (0.4455) (0.2599)
      n16 = Normal3 (0.7896) (0.4455) (0.422)
      n17 = Normal3 (0.6921) (0.4455) (0.568)
      n18 = Normal3 (0.568) (0.4455) (0.6921)
      n19 = Normal3 (0.422) (0.4455) (0.7896)
      n20 = Normal3 (0.2599) (0.4455) (0.8567)
      n21 = Normal3 (0.0878) (0.4455) (0.891)
      n22 = Normal3 (-0.0878) (0.4455) (0.891)
      n23 = Normal3 (-0.2599) (0.4455) (0.8567)
      n24 = Normal3 (-0.422) (0.4455) (0.7896)
      n25 = Normal3 (-0.568) (0.4455) (0.6921)
      n26 = Normal3 (-0.6921) (0.4455) (0.568)
      n27 = Normal3 (-0.7896) (0.4455) (0.422)
      n28 = Normal3 (-0.8567) (0.4455) (0.2599)
      n29 = Normal3 (-0.891) (0.4455) (0.0878)
      n30 = Normal3 (-0.891) (0.4455) (-0.0878)
      n31 = Normal3 (-0.8567) (0.4455) (-0.2599)
      n32 = Normal3 (-0.7896) (0.4455) (-0.422)
      n33 = Normal3 (-0.6921) (0.4455) (-0.568)
      n34 = Normal3 (-0.568) (0.4455) (-0.6921)
      n35 = Normal3 (-0.422) (0.4455) (-0.7896)
      n36 = Normal3 (-0.0) (-1.0) (-0.0)
      n37 = Normal3 (-0.2599) (0.4455) (-0.8567)
      n38 = Normal3 (-0.0878) (0.4455) (-0.891)
  renderPrimitive Polygon $ do
      drawPolygon n6 v8 v40 v9 
      drawPolygon n7 v9 v40 v10 
      drawPolygon n8 v10 v40 v11 
      drawPolygon n9 v11 v40 v12 
      drawPolygon n10 v12 v40 v13 
      drawPolygon n11 v13 v40 v14 
      drawPolygon n12 v14 v40 v15 
      drawPolygon n13 v15 v40 v16 
      drawPolygon n14 v16 v40 v17 
      drawPolygon n15 v17 v40 v18 
      drawPolygon n16 v18 v40 v19 
      drawPolygon n17 v19 v40 v20 
      drawPolygon n18 v20 v40 v21 
      drawPolygon n19 v21 v40 v22 
      drawPolygon n20 v22 v40 v23 
      drawPolygon n21 v23 v40 v24 
      drawPolygon n22 v24 v40 v25 
      drawPolygon n23 v25 v40 v26 
      drawPolygon n24 v26 v40 v27 
      drawPolygon n25 v27 v40 v28 
      drawPolygon n26 v28 v40 v29 
      drawPolygon n27 v29 v40 v30 
      drawPolygon n28 v30 v40 v31 
      drawPolygon n29 v31 v40 v32 
      drawPolygon n30 v32 v40 v33 
      drawPolygon n31 v33 v40 v34 
      drawPolygon n32 v34 v40 v35 
      drawPolygon n33 v35 v40 v36 
      drawPolygon n34 v36 v40 v37 
      drawPolygon n35 v37 v40 v38 
      drawPolygon n37 v38 v40 v39 
      drawPolygon n38 v39 v40 v8 
  renderPrimitive Quads $ do
      drawFace n0 v0 v1 v3 v2 
      drawFace n1 v2 v3 v7 v6 
      drawFace n2 v6 v7 v5 v4 
      drawFace n3 v4 v5 v1 v0 
      drawFace n4 v2 v6 v4 v0 
      drawFace n5 v7 v3 v1 v5 

drawTriangle :: Normal3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> IO ()
drawTriangle p q r s = do
    let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
    normal p
    texCoord2f (TexCoord2 1 1)
    vertex q
    texCoord2f (TexCoord2 0 1)
    vertex r
    texCoord2f (TexCoord2 0 0)
    vertex s

-- Fungsi untuk menggambar polygon dengan parameter normal dan vertex
drawPolygon :: Normal3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> IO ()
drawPolygon normalParam vertex1 vertex2 vertex3 = do
    renderPrimitive Polygon $ do
        normal normalParam
        vertex vertex1
        vertex vertex2
        vertex vertex3

display :: State -> DisplayCallback
display state = do
   loadIdentity
   translate (Vector3 0 0 (-5 :: GLfloat))

   Vector3 xDiff yDiff zDiff <- get (diff state)
   rotate yDiff (Vector3 1 0 0)
   rotate xDiff (Vector3 0 1 0)
   rotate zDiff (Vector3 0 0 1)

   sc <- get (theScale state)
   scale sc sc sc

   clear [ ColorBuffer, DepthBuffer ]
   (drawModel:_) <- get (modelCycle state)
   drawModel

   flush
   swapBuffers

nextClearColor :: State -> IO ()
nextClearColor state = do
   cc <- get (colorCycle state)
   clearColor $= head cc
   colorCycle state $~ tail

toggleRotation :: State -> IO ()
toggleRotation state = do
   rot <- get (shouldRotate state)
   shouldRotate state $~ not
   if rot
      then do
         ia <- get (inertia state)
         inertiaOld state $= ia
      else do
        io <- get (inertiaOld state)
        inertia state $= io
        -- To prevent confusion, force some rotation
        when (dot io io == 0) $
           inertia state $= initialInertia

printHelp :: IO ()
printHelp = mapM_ putStrLn [
   "",
   "Keyboard commands:",
   "",
   "b - Toggle among background clear colors",
   "q, <esc> - Quit",
   "t - Toggle among models to render",
   "? - Help",
   "<home> - reset zoom and rotation",
   "<space> or <click> - stop rotation",
   "<+>, <-> or <ctrl + drag> - zoom model",
   "<arrow keys> or <drag> - rotate model",
   ""]

resetState :: State -> IO ()
resetState state = do
   diff state $= initialDiff
   lastIncr state $= pure 0
   inertia state $= initialInertia
   theScale state $= 1

calcInertia :: State -> IO ()
calcInertia state = do
   lastPosition state $= Position (-1) (-1)
   li <- get (lastIncr state)
   ia <- get (inertia state)
   let t = pure inertiaThreshold
       f = pure inertiaFactor
       l = (pure 1 $- (step (fmap negate t) li)) $* ((li $+ t) $* f $- ia)
       r = (step t li) $* ((li $- t) $* f $- ia)
   inertia state $= l $+ ia $+ r
   lastIncr state $= pure 0

keyboard :: State -> KeyboardMouseCallback
keyboard state key keyState mods _ = do
   modifiers state $= mods
   postRedisplay Nothing
   case (key, keyState) of
      (Char 'b', Down) -> nextClearColor state
      (Char 'q', Down) -> exitWith ExitSuccess
      (Char '\27', Down) -> exitWith ExitSuccess
      (Char 't', Down) -> modelCycle state $~ tail
      (Char ' ', Down) -> toggleRotation state
      (Char '+', Down) -> theScale state $~ (+ scaleIncrement)
      (Char '-', Down) -> theScale state $~ (+ (- scaleIncrement))
      (Char _, Down) -> printHelp
      (SpecialKey KeyHome, Down) -> resetState state
      (SpecialKey KeyLeft, Down) -> diff state $~ ($- Vector3 1 0 0)
      (SpecialKey KeyRight, Down) -> diff state $~ ($+ Vector3 1 0 0)
      (SpecialKey KeyUp, Down) -> diff state $~ ($- Vector3 0 1 0)
      (SpecialKey KeyDown, Down) -> diff state $~ ($+ Vector3 0 1 0)
      (MouseButton LeftButton, Down) -> do
         inertia state $= pure 0
         lastIncr state $= pure 0
      (MouseButton LeftButton, Up) -> calcInertia state
      (_, _) -> return ()

motion :: State -> MotionCallback
motion state pos@(Position x y) = do
   postRedisplay Nothing
   Position xt yt <- get (lastPosition state)
   lastPosition state $= pos
   when (xt /= -1 || yt /= -1) $ do
      let li@(Vector3 xl yl _) = Vector3 (fromIntegral (x - xt)) (fromIntegral (y - yt)) 0
      lastIncr state $= li
      when (xt /= -1) $ do
         mods <- get (modifiers state)
         if ctrl mods == Down
            then do diff state $~ ($+ Vector3 0 0 xl)
                    theScale state $~ (+ (yl * scaleFactor))
            else diff state $~ ($+ li)

timer :: State -> TimerCallback
timer state = do
   rot <- get (shouldRotate state)
   when rot $ do
      ia <- get (inertia state)
      diff state $~ ($+ ia)
      postRedisplay Nothing
   addTimerCallback timerFrequencyMillis (timer state)

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   let vp = 0.8
       aspect = fromIntegral w / fromIntegral h

   viewport $= (Position 0 0, size)

   matrixMode $= Projection
   loadIdentity
   frustum (-vp) vp (-vp / aspect) (vp / aspect) 3 10

   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-5 :: GLfloat))

-- Make sure that GLSL is supported by the driver, either directly by the core
-- or via an extension.
checkGLSLSupport :: IO ()
checkGLSLSupport = do
   version <- get (majorMinor glVersion)
   unless (version >= (2,0)) $ do
      extensions <- get glExtensions
      unless ("GL_ARB_shading_language_100" `elem` extensions) $
         ioError (userError "No GLSL support found.")

readAndCompileShader :: ShaderType -> FilePath -> IO Shader
readAndCompileShader st filePath = do
   src <- B.readFile filePath
   shader <- createShader st
   shaderSourceBS shader $= src
   compileShader shader
   reportErrors
   ok <- get (compileStatus shader)
   infoLog <- get (shaderInfoLog shader)
   mapM_ putStrLn ["Shader info log for '" ++ filePath ++ "':", infoLog, ""]
   unless ok $ do
      deleteObjectNames [shader]
      ioError (userError "shader compilation failed")
   return shader

installBrickShaders :: [Shader] -> IO ()
installBrickShaders shaders = do
   brickProg <- createProgram
   attachedShaders brickProg $= shaders
   linkProgram brickProg
   reportErrors
   ok <- get (linkStatus brickProg)
   infoLog <- get (programInfoLog brickProg)
   mapM_ putStrLn ["Program info log:", infoLog, ""]
   unless ok $ do
      deleteObjectNames [brickProg]
      ioError (userError "linking failed")

   currentProgram $= Just brickProg

   let setUniform var val = do
       location <- get (uniformLocation brickProg var)
       reportErrors
       uniform location $= val

   setUniform "BrickColor" (Color3 1.0 0.3 (0.2 :: GLfloat))
   setUniform "MortarColor" (Color3 0.85 0.86 (0.84 :: GLfloat))
   setUniform "BrickSize" (Vertex2 0.30 (0.15 :: GLfloat))
   setUniform "BrickPct" (Vertex2 0.90 (0.85 :: GLfloat))
   setUniform "LightPosition" (Vertex3 0 0 (4 :: GLfloat))

main :: IO ()
main = do
   _ <- getArgsAndInitialize
   initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
   initialWindowSize $= Size 500 500
   _ <- createWindow "3Dlabs Brick Shader"

   -- Note: We don't use an idle callback, we redisplay more intelligently.
   state <- makeState
   displayCallback $= display state
   keyboardMouseCallback $= Just (keyboard state)
   reshapeCallback $= Just reshape
   motionCallback $= Just (motion state)
   addTimerCallback timerFrequencyMillis (timer state)

   Control.Exception.catch
     (do checkGLSLSupport
         vs <- readAndCompileShader VertexShader "Brick.vert"
         fs <- readAndCompileShader FragmentShader "Brick.frag"
         installBrickShaders [vs, fs])
     (\exception -> do
         print (exception :: IOException)
         putStrLn "Using fixed function pipeline."
         materialDiffuse Front $= Color4 1 0.3 0.2 1
         materialSpecular Front $= Color4 0.3 0.3 0.3 1
         materialShininess Front $= 16
         position (Light 0) $= Vertex4 0 0 4 0
         lighting $= Enabled
         light (Light 0) $= Enabled)

   depthFunc $= Just Less
   nextClearColor state

   -- display help
   keyboard state (Char '?') Down (Modifiers Up Up Up) (Position 0 0)

   mainLoop
