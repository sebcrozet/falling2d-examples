import System.Random
import GHC.Float
import Data.Vect.Double.Base hiding(translation)
import Physics.Falling.World.GenericWorld
import Physics.Falling.RigidBody.OrderedRigidBody
import Physics.Falling.RigidBody.RigidBody
import Physics.Falling.RigidBody.DynamicBody
import Physics.Falling.RigidBody.Dynamic
import Physics.Falling.RigidBody.StaticBody
import qualified Physics.Falling.RigidBody.Positionable as RB (translate)
import Physics.Falling.Shape.Ball
import Physics.Falling.Shape.Plane
import Physics.Falling2d.Rectangle
import Physics.Falling2d.World2d
import Physics.Falling2d.Shape2d
import Physics.Falling2d.Vec1
import Physics.Falling2d.RigidBody2d

import Render

main :: IO ()
main = runSimulate worldInit

worldInit :: DefaultWorld2d Int
worldInit = addRigidBodies (balls ++ rects ++ ground) $ mkWorld2d
            where
            balls  = [] -- map (generateDynamicBody (Ball2d $ ball 1.0)) [1 .. 40]
            rects  = map (generateDynamicBody (Rectangle2d $ Rectangle 1.0 1.0)) [10 .. 35] -- 6 .. 26]
            ground = [
                      orderRigidBody (-1) $ StaticBody $ RB.translate (Vec2 0 (-5))
                                                       $ mkStaticBody idmtx (Plane2d $ planev $ Vec2 (1) 1)
                      , orderRigidBody (-2) $ StaticBody $ RB.translate (Vec2 0 (-5))
                                                         $ mkStaticBody idmtx (Plane2d $ planev $ Vec2 (-1) 1)
                     ]

generateDynamicBody :: DynamicShape2d -> Int -> OrderedRigidBody2d Int
generateDynamicBody s i = orderRigidBody i $ DynamicBody
                                           $ RB.translate (Vec2 fdx $ fdy + 10)
                                           $ setExternalLinearForce (Vec2 0.0 (-9.81))
                                           -- $ setExternalAngularForce (Vec1 (-pi))
                                           $ mkDynamicBody idmtx s 1.0 (Vec2 0.0 0.0) (Vec1 0.0)
                        where
                        g = mkStdGen (i * 20)
                        (_, g')   = next g
                        (dx, g'') = next g'
                        (dy, _)   = next g''
                        (m, mx)    = genRange g
                        range = int2Double mx - int2Double m
                        fdx = int2Double dx / range * 30.0 - 15.0
                        fdy = int2Double dy / range * 30.0 - 15.0
