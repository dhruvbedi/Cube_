module Main where

import Graphics.Isometric
import Partial.Unsafe
import Prelude

import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.JQuery (off)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef, Ref)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.HTMLProgressElement (position)
import DOM.HTML.Types (Window)
import DOM.HTML.Window (requestAnimationFrame)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, CanvasElement, Context2D, beginPath, clearRect, closePath, fillRect, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, lineTo, moveTo, rotate, setFillStyle, setStrokeStyle, stroke, withContext)
import Math (pi, cos, sin)

--foreign import clearCanvas :: Unit

-- Define 3D and 2D Point objects
newtype Point3 = Point3
  { x :: Number
  , y :: Number
  , z :: Number
  }

newtype Point2 = Point2
  { x :: Number
  , y :: Number
  }

newtype Angle = Angle
  { qx :: Number
  , qy :: Number
  , qz :: Number
  }

-- Define cube object
newtype Cube = Cube
  { x :: Number
  , y :: Number
  , z :: Number
  , size :: Number
  , color :: String
  }


-- Function to project 3D point on 2D coordinate plane

project :: Point3 -> Angle -> Point2
project (Point3 { x, y, z }) (Angle { qx, qy, qz }) =
  let xRotQz = x * (cos qz) + y * (sin qz)
      yRotQz = y * (cos qz) - x * (sin qz)
      yRotQzQx = yRotQz * (cos qx) + z * (sin qx)
      zRotQzQx = z * (cos qx) - yRotQz * (sin qx)
      xRotQzQxQy = xRotQz * (cos qy) + zRotQzQx * (sin qy)
  in
    Point2 { x: xRotQzQxQy, y: yRotQzQx }


withStroke :: forall e.
  Context2D ->
  String ->
  (Context2D -> Eff (canvas :: CANVAS | e) Context2D) ->
  Eff (canvas :: CANVAS | e) Context2D
withStroke ctx color draw = withContext ctx do
  ctx <- setStrokeStyle color ctx
  ctx <- beginPath ctx
  ctx <- draw ctx
  ctx <- closePath ctx
  stroke ctx


drawLine :: forall e. Context2D -> Point2 -> Point2 -> Eff (canvas :: CANVAS | e) Context2D
drawLine ctx (Point2 from) (Point2 to) = do
  ctx <- moveTo ctx from.x from.y
  lineTo ctx to.x to.y


drawCube :: forall e. Context2D -> Cube -> Angle -> Eff (canvas :: CANVAS | e) Context2D
drawCube ctx (Cube { color, x, y, z, size }) (Angle { qx, qy, qz })= do
  let half = size / 2.0
  let v1 = project (Point3 { x: x - half, y: y - half, z: z - half }) (Angle {qx: qx, qy: qy, qz: qz})
  let v2 = project (Point3 { x: x - half, y: y + half, z: z - half }) (Angle {qx: qx, qy: qy, qz: qz})
  let v3 = project (Point3 { x: x - half, y: y - half, z: z + half }) (Angle {qx: qx, qy: qy, qz: qz})
  let v4 = project (Point3 { x: x - half, y: y + half, z: z + half }) (Angle {qx: qx, qy: qy, qz: qz})
  let v5 = project (Point3 { x: x + half, y: y - half, z: z - half }) (Angle {qx: qx, qy: qy, qz: qz})
  let v6 = project (Point3 { x: x + half, y: y + half, z: z - half }) (Angle {qx: qx, qy: qy, qz: qz})
  let v7 = project (Point3 { x: x + half, y: y - half, z: z + half }) (Angle {qx: qx, qy: qy, qz: qz})
  let v8 = project (Point3 { x: x + half, y: y + half, z: z + half }) (Angle {qx: qx, qy: qy, qz: qz})
  withStroke ctx color \ctx -> do
    ctx <- drawLine ctx v1 v5
    ctx <- drawLine ctx v5 v6
    ctx <- drawLine ctx v6 v2
    ctx <- drawLine ctx v2 v1

    ctx <- drawLine ctx v3 v7
    ctx <- drawLine ctx v7 v8
    ctx <- drawLine ctx v8 v4
    ctx <- drawLine ctx v4 v3

    ctx <- drawLine ctx v1 v3
    ctx <- drawLine ctx v5 v7
    ctx <- drawLine ctx v6 v8
    drawLine ctx v2 v4

clearCanvas ::forall e.Eff (canvas :: CANVAS | e) Unit
clearCanvas  = do
  val <- getCanvasElementById "thecanvas"
  case  val of
    Just canvas -> do
      width <- getCanvasWidth canvas
      height <- getCanvasHeight canvas
      ctx <- getContext2D canvas
      void $ clearRect ctx { x: 0.0, y: 0.0, w: width, h: height }
    Nothing -> pure unit

forTen :: forall e.Context2D -> Int -> Eff (canvas :: CANVAS | e) Context2D
forTen ctx pos = do
  _ <- clearCanvas
  case pos of
    0 -> drawCube ctx (Cube { x: 300.0, y: 500.0, z: 250.0, size: 200.0, color: "rgb(0,1,0)" }) (Angle { qx: pi/(4.0 ), qy: pi/(3.0+ (toNumber pos)+10.0), qz: pi/4.0})
    _ -> do
      _ <- forTen ctx (pos-1)
      drawCube ctx (Cube { x: 300.0, y: 500.0, z: 250.0, size: 200.0, color: "rgb(0,1,0)" }) (Angle { qx: pi/(4.0 ) , qy: pi/(3.0+ (toNumber pos)+10.0), qz: pi/4.0})

main :: forall e. Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "thecanvas"
  ctx <- getContext2D canvas
  forTen ctx 10
  -- i <-pure $ 1
  -- _<-drawCube ctx (Cube { x: 300.0, y: 500.0, z: 250.0, size: 200.0, color: "rgb(0,1,0)" }) (Angle { qx: pi/(4.0 + (toNumber i)*10.0) , qy: pi/3.0, qz: pi/4.0})
  -- _ <- clearCanvas canvas
  -- i <-pure $ 3
  -- drawCube ctx (Cube { x: 300.0, y: 500.0, z: 250.0, size: 200.0, color: "rgb(0,1,0)" }) (Angle { qx: pi/(4.0 + (toNumber i)*10.0) , qy: pi/3.0, qz: pi/4.0})
