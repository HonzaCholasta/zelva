module RenderWidget where

import Qtc.Classes.Qccs hiding (clear, flush, name, value)
import Qtc.Classes.Qccs_h
import Qtc.Classes.Core
import Qtc.Classes.Gui hiding (color, light, position, rotate, viewport)
import Qtc.Classes.Opengl
import Qtc.ClassTypes.Gui
import Qtc.ClassTypes.Opengl
import Qtc.Core.Base
import Qtc.Core.QVariant
import Qtc.Enums.Core.Qt
import Qtc.Gui.QMessageBox
import Qtc.Gui.QMouseEvent
import Qtc.Enums.Gui.QMessageBox
import Qtc.Opengl.QGLWidget
import Qtc.Opengl.QGLWidget_h

import Graphics.Rendering.OpenGL.GL
--import Graphics.Rendering.OpenGL.GL.BasicTypes
--import Graphics.Rendering.OpenGL.GL.BeginEnd
--import Graphics.Rendering.OpenGL.GL.Colors
--import Graphics.Rendering.OpenGL.GL.CoordTrans
--import Graphics.Rendering.OpenGL.GL.FlushFinish
--import Graphics.Rendering.OpenGL.GL.Framebuffer
--import Graphics.Rendering.OpenGL.GL.Hints
--import Graphics.Rendering.OpenGL.GL.Polygons
--import Graphics.Rendering.OpenGL.GL.StateVar
--import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GLU.Matrix
import Graphics.Rendering.OpenGL.GLU.Quadrics

import Turtle

colors = [
    Color4 (0.8 :: GLfloat) 0.498039 0.196078 1.0,
    Color4 (0.5 :: GLfloat) 0.5 0.5 1.0,
    Color4 (1.0 :: GLfloat) 0.0 0.0 1.0,
    Color4 (1.0 :: GLfloat) 1.0 0.0 1.0,
    Color4 (0.2 :: GLfloat) 0.7 0.1 1.0,
    Color4 (0.0 :: GLfloat) 1.0 1.0 1.0,
    Color4 (0.0 :: GLfloat) 0.0 1.0 1.0,
    Color4 (1.0 :: GLfloat) 0.0 1.0 1.0,
    Color4 (0.439216 :: GLfloat) 0.858824 0.576471 1.0,
    Color4 (1.0 :: GLfloat) 0.498039 0.0 1.0,
    Color4 (0.258824 :: GLfloat) 0.258824 0.435294 1.0,
    Color4 (0.6 :: GLfloat) 0.196078 0.8 1.0,
    Color4 (0.439216 :: GLfloat) 0.576471 0.858824 1.0,
    Color4 (0.556863 :: GLfloat) 0.137255 0.137255 1.0,
    Color4 (0.858824 :: GLfloat) 0.858824 0.439216 1.0,
    Color4 (0.623529 :: GLfloat) 0.623529 0.372549 1.0]

renderWidget :: QWidget t1 -> Maybe Turtle -> IO (QGLWidget ())
renderWidget parent turtle = do
    this <- qGLWidget (parent)
    setHandler this "initializeGL()" $ renderWidget_initializeGL
    setHandler this "resizeGL(int,int)" $ renderWidget_resizeGL
    setHandler this "paintGL()" $ renderWidget_paintGL turtle
    setHandler this "mousePressEvent(QMouseEvent*)" $ renderWidget_mousePressEvent
    setHandler this "mouseMoveEvent(QMouseEvent*)" $ renderWidget_mouseMoveEvent
    qObjectSetProperty this "lastx" =<< qVariant (0 :: Int)
    qObjectSetProperty this "lasty" =<< qVariant (0 :: Int)
    qObjectSetProperty this "mode" =<< qVariant (0 :: Int)
    qObjectSetProperty this "xangle" =<< qVariant (0 :: Int)
    qObjectSetProperty this "yangle" =<< qVariant (0 :: Int)
    qObjectSetProperty this "zdist" =<< qVariant (0 :: Int)
    return this

renderWidget_update :: QGLWidget () -> (Maybe Turtle) -> IO ()
renderWidget_update this turtle = do
    setHandler this "paintGL()" $ renderWidget_paintGL turtle
    updateGL this ()
    return ()

renderWidget_initializeGL :: QGLWidget () -> IO ()
renderWidget_initializeGL this = do
    hint PerspectiveCorrection $= Nicest
    polygonMode $= (Fill, Fill)
    cullFace $= Nothing

    lighting $= Enabled
    lightModelTwoSide $= Enabled

    light (Light 0) $= Enabled
    ambient (Light 0) $= Color4 0.5 0.5 0.5 1.0
    diffuse (Light 0) $= Color4 0.9 0.9 0.9 1.0
    specular (Light 0) $= Color4 1.0 1.0 1.0 1.0
    position (Light 0) $= Vertex4 1.0 1.0 1.0 0.0
    materialSpecular FrontAndBack $= Color4 1.0 1.0 1.0 1.0
    materialShininess FrontAndBack $= 128

    return ()

renderWidget_resizeGL :: QGLWidget () -> Int -> Int -> IO ()
renderWidget_resizeGL this width height = do
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    matrixMode $= Projection
    loadIdentity
    perspective 45.0 ((fromIntegral width) / (fromIntegral height)) 1.0 1000.0
    matrixMode $= Modelview 0
    return ()

renderWidget_paintGL :: Maybe Turtle -> QGLWidget () -> IO ()
renderWidget_paintGL turtle this = do
    clear [ColorBuffer, DepthBuffer]
    matrixMode $= Modelview 0
    loadIdentity

    xangle <- qVariantValue_Int =<< qObjectProperty this "xangle"
    yangle <- qVariantValue_Int =<< qObjectProperty this "yangle"
    zdist <- qVariantValue_Int =<< qObjectProperty this "zdist"

    translate (Vector3 0.0 0.0 (-50.0 :: GLfloat))
    translate (Vector3 0.0 0.0 ((fromIntegral zdist) :: GLfloat))
    rotate ((fromIntegral xangle) :: GLfloat) (Vector3 1.0 0.0 0.0)
    rotate ((fromIntegral yangle) :: GLfloat) (Vector3 0.0 1.0 0.0)

    let
        qstyle = QuadricStyle (Just Smooth) NoTextureCoordinates Outside FillStyle

        renderTurtle :: Turtle -> IO ()
        renderTurtle t = do
            materialAmbientAndDiffuse FrontAndBack $= colors !! 0
            doCommandList t (recursion t) (TurtleState (angle t) (thickness t) 0, axiom t)
            return ()

        doCommandList :: Turtle -> Int -> (TurtleState, [TurtleCommand]) -> IO (TurtleState, [TurtleCommand])
        doCommandList tu d s@(_, []) = return s
        doCommandList tu d s = (doCommand tu d s) >>= (doCommandList tu d)

        doCommand :: Turtle -> Int -> (TurtleState, [TurtleCommand]) -> IO (TurtleState, [TurtleCommand])
        doCommand tu d s@(st, ((TurtleCommand cn _):cs)) =
            maybe (renderCommand tu d s) (\cl -> doCommandList tu (d-1) (st, cl) >>= cont) (if d > 0 then ruleLookup tu cn else Nothing)
            where cont (nst, _) = return (nst, cs)

        -- forward: z, left: x, up: y
        renderCommand :: Turtle -> Int -> (TurtleState, [TurtleCommand]) -> IO (TurtleState, [TurtleCommand])
        renderCommand tu d (st@(TurtleState ang th col), ((TurtleCommand cn val):cs)) =
            case cn of
                '+' -> do
                    rotate (realToFrac (maybe ang id val)) $ Vector3 (0.0 :: GLfloat) 1.0 0.0
                    return (st, cs)
                '-' -> do
                    rotate (-realToFrac (maybe ang id val)) $ Vector3 (0.0 :: GLfloat) 1.0 0.0
                    return (st, cs)
                '&' -> do
                    rotate (realToFrac (maybe ang id val)) $ Vector3 (1.0 :: GLfloat) 0.0 0.0
                    return (st, cs)
                '^' -> do
                    rotate (-realToFrac (maybe ang id val)) $ Vector3 (1.0 :: GLfloat) 0.0 0.0
                    return (st, cs)
                '<' -> do
                    rotate (realToFrac (maybe ang id val)) $ Vector3 (0.0 :: GLfloat) 0.0 1.0
                    return (st, cs)
                '>' -> do
                    rotate (-realToFrac (maybe ang id val)) $ Vector3 (0.0 :: GLfloat) 0.0 1.0
                    return (st, cs)
                '|' -> do
                    rotate 180.0 $ Vector3 (0.0 :: GLfloat) 1.0 0.0
                    return (st, cs)
                '%' -> do
                    rotate 180.0 $ Vector3 (0.0 :: GLfloat) 0.0 1.0
                    return (st, cs)
                '$' -> do
                    return (st, cs)
                '~' -> do
                    return (st, cs)
                't' -> do
                    return (st, cs)
                'F' -> do
                    cullFace $= Just Back
                    renderQuadric qstyle $ Cylinder (realToFrac th / 2) (realToFrac th / 2) (maybe 1.0 realToFrac val) 8 1
                    translate $ Vector3 (0.0 :: GLfloat) 0.0 (maybe 1.0 realToFrac val)
                    return (st, cs)
                'Z' -> do
                    cullFace $= Just Back
                    renderQuadric qstyle $ Cylinder (realToFrac th / 2) (realToFrac th / 2) (maybe 0.5 realToFrac val) 8 1
                    translate $ Vector3 (0.0 :: GLfloat) 0.0 (maybe 1.0 realToFrac val)
                    return (st, cs)
                'f' -> do
                    vertex $ Vertex3 (0.0 :: GLfloat) 0.0 (maybe 1.0 realToFrac val)
                    translate $ Vector3 (0.0 :: GLfloat) 0.0 (maybe 1.0 realToFrac val)
                    return (st, cs)
                'z' -> do
                    vertex $ Vertex3 (0.0 :: GLfloat) 0.0 (maybe 0.5 realToFrac val)
                    translate $ Vector3 (0.0 :: GLfloat) 0.0 (maybe 1.0 realToFrac val)
                    return (st, cs)
                'g' -> do
                    translate $ Vector3 (0.0 :: GLfloat) 0.0 (maybe 1.0 realToFrac val)
                    return (st, cs)
                '.' -> do
                    vertex $ Vertex3 (0.0 :: GLfloat) 0.0 0.0
                    return (st, cs)
                '[' -> do
                    preservingMatrix $ doCommandList tu d (st, takeWhile (\x -> (name x) /= ']') cs)
                    return (st, dropWhile (\x -> (name x) /= ']') cs)
                '{' -> do
                    cullFace $= Nothing
                    renderPrimitive TriangleFan $ doCommandList tu d (st, takeWhile (\x -> (name x) /= '}') cs)
                    return (st, dropWhile (\x -> (name x) /= '}') cs)
                '"' -> do
                    scale (1.0 :: GLfloat) 1.0 (maybe 1.1 realToFrac val)
                    return (st, cs)
                '\'' -> do
                    scale (1.0 :: GLfloat) 1.0 (maybe 0.9 realToFrac val)
                    return (st, cs)
                ';' -> do
                    return (st { curAngle = ang * (maybe 1.1 realToFrac val) }, cs)
                ':' -> do
                    return (st { curAngle = ang * (maybe 0.9 realToFrac val) }, cs)
                '?' -> do
                    return (st { curThickness = th * (maybe 1.4 realToFrac val) }, cs)
                '!' -> do
                    return (st { curThickness = th * (maybe 0.7 realToFrac val) }, cs)
                'c' -> do
                    let ncol = (maybe (col+1) truncate val) `mod` (length colors)
                    materialAmbientAndDiffuse FrontAndBack $= colors !! ncol
                    return (st { colorIndex = ncol } , cs)
                '*' -> do
                    return (st, cs)
                _   -> do
                    return (st, cs)


        {-    case (name cmd) of
                '+' -> rotate (realToFrac (maybe (angle turtle) id (value cmd))) $ Vector3 (0.0 :: Float) 1.0 0.0
                '-' -> rotate (-realToFrac (maybe (angle turtle) id (value cmd))) $ Vector3 (0.0 :: Float) 1.0 0.0
                '&' -> rotate (realToFrac (maybe (angle turtle) id (value cmd))) $ Vector3 (1.0 :: Float) 0.0 0.0
                '^' -> rotate (-realToFrac (maybe (angle turtle) id (value cmd))) $ Vector3 (1.0 :: Float) 0.0 0.0
                '<' -> rotate (realToFrac (maybe (angle turtle) id (value cmd))) $ Vector3 (0.0 :: Float) 0.0 1.0
                '>' -> rotate (-realToFrac (maybe (angle turtle) id (value cmd))) $ Vector3 (0.0 :: Float) 0.0 1.0
                '|' -> rotate 180.0 $ Vector3 (0.0 :: Float) 1.0 0.0
                '%' -> rotate 180.0 $ Vector3 (0.0 :: Float) 0.0 1.0
                '$' -> return ()
                '~' -> return ()
                't' -> return ()
                'F' -> do
                    cullFace $= Just Back
                    renderQuadric qstyle $ Cylinder (realToFrac (thickness turtle) / 2) (realToFrac (thickness turtle) / 2) (maybe 1.0 realToFrac (value cmd)) 8 1
                    translate $ Vector3 (0.0 :: Float) 0.0 (maybe 1.0 realToFrac (value cmd))
                'Z' -> do
                    cullFace $= Just Back
                    renderQuadric qstyle $ Cylinder (realToFrac (thickness turtle) / 2) (realToFrac (thickness turtle) / 2) (maybe 0.5 realToFrac (value cmd)) 8 1
                    translate $ Vector3 (0.0 :: Float) 0.0 (maybe 1.0 realToFrac (value cmd))
                'f' -> return ()
                'z' -> return ()
                'g' -> translate $ Vector3 (0.0 :: Float) 0.0 (maybe 1.0 realToFrac (value cmd))
                '.' -> return ()
                '[' -> do
                    result <- preservingMatrix $ renderCommands
                    return (s, snd result)
                ']' -> return ()
                '{' -> return ()
                '}' -> return ()
                '"' -> scale (1.0 :: Float) 1.0 (maybe 1.1 realToFrac (value cmd))
                '\''-> scale (1.0 :: Float) 1.0 (maybe 0.9 realToFrac (value cmd))
                ';' -> return ()
                ':' -> return ()
                '?' -> scale (maybe 1.4 realToFrac (value cmd)) (maybe 1.4 realToFrac (value cmd)) (1.0 :: Float)
                '!' -> scale (maybe 0.7 realToFrac (value cmd)) (maybe 0.7 realToFrac (value cmd)) (1.0 :: Float)
                'c' -> return ()
                '*' -> return ()
                _   -> return ()-}

    maybe (return ()) renderTurtle turtle

{-    renderPrimitive Quads $ do
        color $ colors !! 0
        vertex $ Vertex3 w w w
        vertex $ Vertex3 w (-w) w
        vertex $ Vertex3 w (-w) (-w)
        vertex $ Vertex3 w w (-w)
        color $ colors !! 1
        vertex $ Vertex3 w w (-w)
        vertex $ Vertex3 w (-w) (-w)
        vertex $ Vertex3 (-w) (-w) (-w)
        vertex $ Vertex3 (-w) w (-w)
        color $ colors !! 2
        vertex $ Vertex3 (-w) w (-w)
        vertex $ Vertex3 (-w) (-w) (-w)
        vertex $ Vertex3 (-w) (-w) w
        vertex $ Vertex3 (-w) w w
        color $ colors !! 3
        vertex $ Vertex3 (-w) w w
        vertex $ Vertex3 (-w) (-w) w
        vertex $ Vertex3 w (-w) w
        vertex $ Vertex3 w w w
        color $ colors !! 4
        vertex $ Vertex3 (-w) w w
        vertex $ Vertex3 w w w
        vertex $ Vertex3 w w (-w)
        vertex $ Vertex3 (-w) w (-w)
        color $ colors !! 5
        vertex $ Vertex3 w (-w) (-w)
        vertex $ Vertex3 w (-w) w
        vertex $ Vertex3 (-w) (-w) w
        vertex $ Vertex3 (-w) (-w) (-w)-}

    {-materialAmbientAndDiffuse FrontAndBack $= colors !! 2
    translate (Vector3 (0.0 :: Float) 0.0 (-10.0))
    renderQuadric qstyle (Cylinder 10.0 10.0 20.0 16 16)-}

    flush
    swapBuffers this ()
    return ()

renderWidget_mousePressEvent :: QGLWidget () -> QMouseEvent () -> IO ()
renderWidget_mousePressEvent this event = do
    btn <- button event ()

    qObjectSetProperty this "lastx" =<< qVariant =<< qx event ()
    qObjectSetProperty this "lasty" =<< qVariant =<< qy event ()

    if (btn == eLeftButton)
        then qObjectSetProperty this "mode" =<< qVariant (1 :: Int)
        else if (btn == eRightButton)
            then qObjectSetProperty this "mode" =<< qVariant (2 :: Int)
            else qObjectSetProperty this "mode" =<< qVariant (0 :: Int)

    return ()

renderWidget_mouseMoveEvent :: QGLWidget () -> QMouseEvent () -> IO ()
renderWidget_mouseMoveEvent this event = do
    lastx <- qVariantValue_Int =<< qObjectProperty this "lastx"
    lasty <- qVariantValue_Int =<< qObjectProperty this "lasty"
    mode <- qVariantValue_Int =<< qObjectProperty this "mode"
    xangle <- qVariantValue_Int =<< qObjectProperty this "xangle"
    yangle <- qVariantValue_Int =<< qObjectProperty this "yangle"
    zdist <- qVariantValue_Int =<< qObjectProperty this "zdist"

    newx <- qx event ()
    newy <- qy event ()

    if (mode == 1)
        then do
            qObjectSetProperty this "xangle" =<< qVariant ((xangle + 2 * (newy - lasty)) `mod` 360)
            qObjectSetProperty this "yangle" =<< qVariant ((yangle + 2 * (newx - lastx)) `mod` 360)
            return ()
        else if (mode == 2)
            then do
                qObjectSetProperty this "zdist" =<< qVariant (zdist + 2 * (lasty - newy))
                return ()
            else return ()

    qObjectSetProperty this "lastx" =<< qVariant newx
    qObjectSetProperty this "lasty" =<< qVariant newy

    updateGL this ()

    return ()

qCast_RenderWidget = qCast_QGLWidget
