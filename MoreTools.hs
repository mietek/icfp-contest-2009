module MoreTools where

import Tools


type Scen = [(Int, RV)]


hohTrans :: (Scen -> RV) -> M -> M -> Scen -> Scen
hohTrans runP r1 r2 scen0 = scen2
  where
    p1a = runP scen0
    p1b = runP (scen0 ++ [(1, (0, 0))])
    v1 = satVelRV p1a p1b
    vd1 = hohVDeltaRV v1 r1 r2
    t = round (hohTime r1 r2)
    scen1 = scen0 ++ [(1, vd1), (t, (0, 0))]
    p2a = runP scen1
    p2b = runP (scen1 ++ [(1, (0, 0))])
    v2 = satVelRV p2a p2b
    vd2 = hohVDeltaRV' v2 r1 r2
    scen2 = scen1 ++ [(1, vd2)]


burnFuel :: (Scen -> (Double, RV)) -> Scen -> Scen
burnFuel runFP scen0 = scen1
  where
    (f, pa) = runFP scen0
    (_, pb) = runFP (scen0 ++ [(1, (0, 0))])
    v = satVelRV pa pb
    f' = f - 0.000001
    vda = extDeltaRV v (f' / 4)
    vdb = extDeltaRV v (f' / (-2))
    scen1 = scen0 ++ [(1, vda), (1, vdb), (1, vda)]
