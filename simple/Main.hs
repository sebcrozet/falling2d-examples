
import System.Random
import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Geometry
import Graphics.Gloss.Interface.Pure.Simulate
import Data.Vect.Double.Base hiding(translation)
import Physics.Falling.World.GenericWorld
import Physics.Falling.RigidBody.OrderedRigidBody
import Physics.Falling.RigidBody.CollisionVolume
import Physics.Falling.RigidBody.Positionable hiding(translate, rotate)
import qualified Physics.Falling.RigidBody.Positionable as RB (translate, rotate)
import Physics.Falling.RigidBody.RigidBody
import Physics.Falling.RigidBody.DynamicBody
import Physics.Falling.RigidBody.Dynamic
import Physics.Falling.RigidBody.StaticBody
import Physics.Falling.Math.Transform
import Physics.Falling.Shape.Ball
import Physics.Falling.Shape.Plane
import Physics.Falling.Collision.Graph.CollisionGraph
import Physics.Falling2d.World2d
import Physics.Falling2d.Shape2d
import Physics.Falling2d.RigidBody2d
import Physics.Falling2d.Vec1

import Debug.Trace

simResolution :: Int
simResolution = 60

drawScale :: Float
drawScale = 10.0

main = simulate 
        (InWindow  "falling2d - right-click-drag rotates"
                   (600, 600)	-- x and y size of window (in pixels).
	                 (10, 10))	  -- position of window
	     black		          -- background color
	     simResolution	    -- simulation resolution  
	     		                --    (number of steps to take for each second of time)
	     worldInit	        -- the initial world.
	     drawWorld	        -- a function to convert the world to a Picture.
	     advanceWorld	      -- a function to advance the world to
	     		                --    the next simulation step.

worldInit = addRigidBodies (bodies ++ ground) $ mkWorld2d
            where
            bodies = map generateDynamicBody [1 .. 50]
            ground = [
                       orderRigidBody (-1) $ StaticBody $ RB.translate (Vec2 0 (-10))
                                                        $ mkStaticBody idmtx (Plane2d $ Plane $ Vec2 (-1) 2)
                       , orderRigidBody (-2) $ StaticBody $ RB.translate (Vec2 0 (-10))
                                                          $ mkStaticBody idmtx (Plane2d $ Plane $ Vec2 1 2)
                     ]

generateDynamicBody i = orderRigidBody i $ DynamicBody
                                         $ RB.translate (Vec2 fdx $ fdy + 10)
                                         $ setExternalLinearForce (Vec2 0.0 (-9.81))
                                         $ mkDynamicBody idmtx (Ball2d $ Ball 1.0) 1.0 (Vec2 0.0 0.0) (Vec1 0.0)
                        where
                        g = mkStdGen (i * 20)
                        (_, g')   = next g
                        (dx, g'') = next g'
                        (dy, _)   = next g''
                        (m, mx)    = genRange g
                        range = int2Double mx - int2Double m
                        fdx = int2Double dx / range * 30.0 - 15.0
                        fdy = int2Double dy / range * 30.0 - 15.0

advanceWorld viewport time world = step 0.016 world

drawWorld :: DefaultWorld2d Int -> Picture
drawWorld world = Pictures $ map drawBody $ rigidBodies world

-- | Draw an actor as a picture.
drawBody :: OrderedRigidBody2d Int -> Picture 
drawBody body
 = case rigidBody body of
   DynamicBody db -> let (Vec2 posX posY) = translation $ getLocalToWorld db in
                     Translate (double2Float posX * drawScale) (double2Float posY * drawScale) $ drawDynamicShape $ getCollisionVolume db
   StaticBody sb -> let (Vec2 posX posY) = translation $ getLocalToWorld sb in
                     Translate (double2Float posX * drawScale) (double2Float posY * drawScale) $ drawStaticShape $ getCollisionVolume sb

drawDynamicShape :: DynamicShape2d -> Picture
drawDynamicShape (Ball2d (Ball radius)) = Color green $ circleContour (double2Float radius * drawScale) 10

drawStaticShape  :: StaticShape2d -> Picture
drawStaticShape (StaticBall2d (Ball radius)) = Color (greyN 0.8) $ circleFilled (double2Float radius * drawScale) 10
drawStaticShape (Plane2d      (Plane (Vec2 nx ny))) = Color (greyN 0.8) $ Line [(-fny * 100.0, fnx * 100.0),
                                                                                (fny * 100.0, -fnx * 100.0)]
                                                      where
                                                      fnx = double2Float nx
                                                      fny = double2Float ny

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
