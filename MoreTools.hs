module MoreTools where

import Tools


frame :: Int -> (Double, Double) -> String
frame s (dx, dy) = show s ++ " " ++ show dx ++ " " ++ show dy ++ "\n"


hohTrans :: (String -> RV) -> M -> M -> String -> String
hohTrans runP r1 r2 i0 = i2
  where
    p1a = runP i0
    p1b = runP (i0 ++ frame 1 (0, 0))
    v1 = satVelRV p1a p1b
    vd1 = hohVDeltaRV v1 r1 r2
    t = round (hohTime r1 r2)
    i1 = i0 ++ frame 1 vd1 ++ frame t (0, 0)
    p2a = runP i1
    p2b = runP (i1 ++ frame 1 (0, 0))
    v2 = satVelRV p2a p2b
    vd2 = hohVDeltaRV' v2 r1 r2
    i2 = i1 ++ frame 1 vd2


burnFuel :: (String -> (Double, RV)) -> String -> String
burnFuel runFP i0 = i1
  where
    (f, pa) = runFP i0
    (_, pb) = runFP (i0 ++ frame 1 (0, 0))
    v = satVelRV pa pb
    f' = f - 0.000001
    vda = extDeltaRV v (f' / 4)
    vdb = extDeltaRV v (f' / (-2))
    i1 = i0 ++ frame 1 vda ++ frame 1 vdb ++ frame 1 vda
