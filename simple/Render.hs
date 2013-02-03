module Render
(
runSimulate
)
where

import GHC.Float
import Graphics.Gloss
import Data.Vect.Double.Base hiding(translation)
import Physics.Falling.World.GenericWorld
import Physics.Falling.RigidBody.Dynamic
import Physics.Falling.RigidBody.OrderedRigidBody
import Physics.Falling.RigidBody.CollisionVolume
import Physics.Falling.RigidBody.Positionable hiding(translate, rotate)
import Physics.Falling.RigidBody.RigidBody
import Physics.Falling.Math.Transform
import Physics.Falling.Shape.Ball
import Physics.Falling.Shape.Plane
import Physics.Falling2d.World2d
import Physics.Falling2d.RigidBody2d
import Physics.Falling2d.Shape2d
import Physics.Falling2d.Rectangle
import Physics.Falling2d.Vec1()

import Debug.Trace

simResolution :: Int
simResolution = 60

drawScale :: Float
drawScale = 10.0

runSimulate :: DefaultWorld2d Int -> IO ()
runSimulate world = simulate 
                   (InWindow  "falling2d - right-click-drag rotates"
                               (600, 600)	-- x and y size of window (in pixels).
	                             (10, 10))	-- position of window
	                 black		              -- background color
	                 simResolution	        -- simulation resolution  
	                 		                    --    (number of steps to take for each second of time)
	                 world    	            -- the initial world.
	                 drawWorld	            -- a function to convert the world to a Picture.
	                 advanceWorld	          -- a function to advance the world to
	                 		                    --    the next simulation step.

advanceWorld :: a -> b -> DefaultWorld2d Int -> DefaultWorld2d Int
advanceWorld _ _ world = step 0.016 world

drawWorld :: DefaultWorld2d Int -> Picture
drawWorld world = Pictures $ map drawBody $ rigidBodies world

-- | Draw an actor as a picture.
drawBody :: OrderedRigidBody2d Int -> Picture 
drawBody body
 = case rigidBody body of
   DynamicBody db -> let (Vec2 posX posY) = translation l2w in
                     -- trace (show (getAngularVelocity db) ++ " − " ++ show (getLinearVelocity db)) $
                     Translate (double2Float posX * drawScale) (double2Float posY * drawScale)
                     $ Rotate ((double2Float $ rotationFromMatrix l2w) * 180.0 / pi)
                     $ drawDynamicShape
                     $ getCollisionVolume db
                     where
                     l2w = getLocalToWorld db
   StaticBody sb -> let (Vec2 posX posY) = translation $ getLocalToWorld sb in
                     Translate (double2Float posX * drawScale) (double2Float posY * drawScale)
                     $ drawStaticShape
                     $ getCollisionVolume sb

drawDynamicShape :: DynamicShape2d -> Picture
drawDynamicShape (Ball2d (Ball radius)) = Color green $ circleContour (double2Float radius * drawScale) 10
drawDynamicShape (Rectangle2d (Rectangle rx ry)) = Color red $ rectangleContour (double2Float rx * drawScale)
                                                                                (double2Float ry * drawScale)

drawStaticShape  :: StaticShape2d -> Picture
drawStaticShape (StaticBall2d (Ball radius)) = Color (greyN 0.8) $ circleFilled (double2Float radius * drawScale) 10
drawStaticShape (StaticRectangle2d (Rectangle rx ry)) = Color (greyN 0.8) $ rectangleContour
                                                                            (double2Float rx * drawScale)
                                                                            (double2Float ry * drawScale)
drawStaticShape (Plane2d      (Plane (Vec2 nx ny))) = Color (greyN 0.8) $ Line [(-fny * 100.0, fnx * 100.0),
                                                                                (fny * 100.0, -fnx * 100.0)]
                                                      where
                                                      fnx = double2Float nx
                                                      fny = double2Float ny

-- drawPoint :: Normal2 -> Picture
-- drawPoint n = let Vec2 x y = fromNormal n in
--               Translate (double2Float x * drawScale) (double2Float y * drawScale) $ Color red $ circleFilled 2 5

-- A list of n points spaced equally around the unit circle.
circlePoints :: Float -> [(Float, Float)]
circlePoints n
 =	map 	(\d -> (cos d, sin d))
		[0, 2*pi / n .. 2*pi]

-- Make a circle of radius r consisting of n lines.
circleFilled :: Float -> Float -> Picture
circleFilled r n
 	= Scale r r
	$ Polygon (circlePoints n)

circleContour :: Float -> Float -> Picture
circleContour r n
 	= Scale r r
	$ Line (circlePoints n)

rectangleFilled :: Float -> Float -> Picture
rectangleFilled rx ry = Polygon $ rectanglePoints rx ry

rectangleContour :: Float -> Float -> Picture
rectangleContour rx ry = Line $ rectanglePoints rx ry

rectanglePoints :: Float -> Float -> [(Float, Float)]
rectanglePoints rx ry = [ (-rx, -ry), (rx, -ry), (rx, ry), (-rx, ry), (-rx, -ry) ]

rotationFromMatrix :: Proj3 -> Double
rotationFromMatrix p = -atan (y / x) -- gloss seems to be left-handed while falling2d is riht-handed
                     where
                     (Mat2 (Vec2 x y) _) = trim $ fromProjective p
