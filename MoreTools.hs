module MoreTools where

import LessTools
import Tools
import VM


type Scen = [(Int, RV)]


stdScen :: Scen -> Scen
stdScen scen = [(1, (0, 0))] ++ scen ++ [(901, (0, 0))]


curSatVelRV :: (State -> RV) -> State -> RV
curSatVelRV getP s = v
  where
    p = getP s
    p' = getP (runStepZ s)
    v = satVelRV p p'


matchVel :: (State -> RV) -> (State -> RV) -> State -> (State, Scen)
matchVel getP getQ s0 = (s1, scen)
  where
    v = curSatVelRV getP s0
    w = curSatVelRV getQ s0
    vd = v `subRV` w
    s1 = runStep vd s0
    scen = [(1, vd)]


hohTrans :: (State -> RV) -> M -> State -> (State, Scen)
hohTrans getP dstR s0 = (s2, scen)
  where
    p0 = getP s0
    srcR = lenRV p0
    v1 = curSatVelRV getP s0
    vd1 = hohVDeltaRV v1 srcR dstR
    t = hohTime srcR dstR
    s1 = runNStepsZ t (runStep vd1 s0)
    v2 = curSatVelRV getP s1
    vd2 = hohVDeltaRV' v2 srcR dstR
    s2 = runStep vd2 s1
    scen = [(1, vd1), (t, (0, 0)), (1, vd2)]
