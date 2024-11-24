import Data.List

-- тип данных для точки
data Point = Point Double Double deriving Show

data PRCircle = PRCircle Point Double deriving Show

-- тип данных для окружности, заданной точкой и точкой
data PPCircle = PPCircle Point Point deriving Show

-- класс, обобщающий заданные ранее окружности
class Circle c where
  center :: c -> Point
  radius :: c -> Double


-- "окружность, заданная точкой и радиусом" имеет тип данных класса "окружность"
instance Circle PRCircle where
  center (PRCircle c _) = c
  radius (PRCircle _ r) = r

-- "окружность, заданная точкой и точкой" имеет тип данных класса "окружность"
instance Circle PPCircle where
  center (PPCircle c _) = c
  radius (PPCircle (Point x1 y1) (Point x2 y2)) = r where
    x0 = (x1 - x2) ^ 2
    y0 = (y1 - y2) ^ 2
    r = sqrt (x0 + y0)


main = do
    let c = center (PRCircle (Point 0 0) 1)
    print(c)
