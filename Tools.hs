module Tools where

import LessTools


-- gravitational constant
g :: Double
g = 6.67428e-11

-- earth mass
earthM :: Kg
earthM = 6.0e24

-- earth radius
earthR :: M
earthR = 6.357e6

-- gravitational force
gForce :: Kg -> Kg -> M -> N
gForce m1 m2 r = (g * m1 * m2) / (r * r)

-- earth gravitational force
earthGForce :: Kg -> M -> N
earthGForce = gForce earthM

-- earth-satellite gravitational force
earthSatGForce :: M -> N
earthSatGForce = earthGForce 1


-- satellite-earth gravitational force
satEarthGForce :: RV -> N
satEarthGForce = earthSatGForce . lenRV

-- satellite-earth angle
satEarthAng :: RV -> Rad
satEarthAng = angRV . revRV

-- satellite-earth gravitational force rectangular vector
satEarthGForceRV :: RV -> RV
satEarthGForceRV p = revRV (pvToRV (satEarthGForce p, satEarthAng p))


-- satellite velocity rectangular vector, with dt = 1
satVelRV :: RV -> RV -> RV
satVelRV p1 p2 = (p2 `subRV` p1) `subRV` (a `divRV` 2)
  where
    a = satEarthGForceRV p1

-- satellite velocity rectangular vector
satVelRV' :: S -> RV -> RV -> RV
satVelRV' dt p1 p2 = ((p2 `subRV` p1) `subRV` (a `mulRV` (dt' * dt') `divRV` 2)) `divRV` dt'
  where
    a = satEarthGForceRV p1
    dt' = fromIntegral dt


-- hohmann velocity delta
hohVDelta :: M -> M -> M
hohVDelta r1 r2 = sqrt ((g * earthM) / r1) * (sqrt ((2 * r2) / (r1 + r2)) - 1)

-- hohmann velocity delta rectangular vector
hohVDeltaRV :: RV -> M -> M -> RV
hohVDeltaRV v2 r1 r2 = extDeltaRV v2 h
  where
    h = hohVDelta r1 r2

-- hohmann velocity delta prime
hohVDelta' :: M -> M -> M
hohVDelta' r1 r2 = sqrt ((g * earthM) / r2) * (1 - sqrt ((2 * r1) / (r1 + r2)))

-- hohmann velocity delta rectangular vector prime
hohVDeltaRV' :: RV -> M -> M -> RV
hohVDeltaRV' v2 r1 r2 = extDeltaRV v2 h'
  where
    h' = hohVDelta' r1 r2

-- hohmann time
hohTime :: M -> M -> S
hohTime r1 r2 = round (pi * sqrt ((r1 + r2)^3 / (8 * g * earthM)))
