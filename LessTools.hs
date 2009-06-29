module LessTools where


type Deg = Double
type Rad = Double
type RV = (Double, Double)
type PV = (Double, Rad)
type PV' = (Double, Deg)

type M = Double
type S = Int
type Kg = Double
type N = Double


infinity :: Double
infinity = 1 / 0


firstOf3 :: (a, b, c) -> a
firstOf3 (a, _, _) = a

secondOf3 :: (a, b, c) -> b
secondOf3 (_, b, _) = b

thirdOf3 :: (a, b, c) -> c
thirdOf3 (_, _, c) = c


firstOf4 :: (a, b, c, d) -> a
firstOf4 (a, _, _, _) = a

secondOf4 :: (a, b, c, d) -> b
secondOf4 (_, b, _, _) = b

thirdOf4 :: (a, b, c, d) -> c
thirdOf4 (_, _, c, _) = c

fourthOf4 :: (a, b, c, d) -> d
fourthOf4 (_, _, _, d) = d


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
divRV :: RV -> Double -> RV
divRV (x, y) a = (x / a, y / a)

-- dot product of rectangular vectors
dotRV :: RV -> RV -> Double
dotRV (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

-- normalize rectangular vector
nrmRV :: RV -> RV
nrmRV v = v `divRV` (lenRV v)

-- absolute angle between vectors
angSubRV :: RV -> RV -> Rad
angSubRV v1 v2 =
  if ad <= pi
    then ad
    else -2 * pi + ad
  where
    a1 = angRV v1
    a2 = angRV v2
    ad = a2 - a1


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
