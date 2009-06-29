module Tools where


type Deg = Double
type Rad = Double
type RV = (Double, Double)
type PV = (Double, Rad)
type PV' = (Double, Deg)

type M = Double
type S = Double
type Kg = Double
type N = Double


-- radians to degrees
radToDeg :: Rad -> Deg
radToDeg r = r * 180 / pi

-- degrees to radians
degToRad :: Deg -> Rad
degToRad d = d * pi / 180


-- reverse rectangular vector
revRV :: RV -> RV
revRV (x, y) = (-x, -y)

-- length of rectangular vector
lenRV :: RV -> Double
lenRV (x, y) = sqrt (x * x + y * y)

-- angle of rectangular vector
angRV :: RV -> Rad
angRV (x, y) | x >= 0 && y >= 0 = atan (y / x)
angRV (x, y) | x < 0 = atan (y / x) + pi
angRV (x, y) = atan (y / x) + 2 * pi

-- add rectangular vectors
addRV :: RV -> RV -> RV
addRV (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- subtract rectangular vector
subRV :: RV -> RV -> RV
subRV (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- multiply rectangular vector by scalar
mulRV :: RV -> Double -> RV
mulRV (x, y) a = (x * a, y * a)

-- divide rectangular vector by scalar
divRV :: (Double, Double) -> Double -> (Double, Double)
divRV (x, y) a = (x / a, y / a)


-- rectangular vector to polar vector
rvToPV :: RV -> PV
rvToPV v = (lenRV v, angRV v)

-- rectangular vector to polar vector with degrees
rvToPV' :: PV -> PV'
rvToPV' v = (lenRV v, radToDeg (angRV v))

-- polar vector to rectangular vector
pvToRV :: PV -> RV
pvToRV (l, a) = (cos a, sin a) `mulRV` l


-- length of polar vector
lenPV :: PV -> Double
lenPV (l, _) = l

-- angle of polar vector
angPV :: PV -> Rad
angPV (_, a) = a

-- extend polar vector by scalar
extPV :: PV -> Double -> PV
extPV (l, a) x = (l + x, a)

-- extend rectangular vector by scalar
extRV :: RV -> Double -> RV
extRV v l = pvToRV (rvToPV v `extPV` l)

-- rectangular vector-scalar extension delta
extDeltaRV :: RV -> Double -> RV
extDeltaRV v l = revRV (v' `subRV` v)
  where
    v' = extRV v l



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
satVelRV p1 p2 = p2 `subRV` p1 `subRV` (a `divRV` 2)
  where
    a = satEarthGForceRV p1

-- satellite velocity rectangular vector
satVelRV' :: S -> RV -> RV -> RV
satVelRV' dt p1 p2 = (p2 `subRV` p1 `subRV` (a `mulRV` (dt * dt) `divRV` 2)) `divRV` dt
  where
    a = satEarthGForceRV p1


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
hohTime r1 r2 = pi * sqrt ((r1 + r2)^3 / (8 * g * earthM))
