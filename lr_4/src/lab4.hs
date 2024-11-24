module Lab_20_12 where

-- Лабораторная работа №4

-- сложение матриц
summ :: Int -> Int -> [[Double]] -> [[Double]] -> [[Double]]
summ i j matr1 matr2 = result where
  result = yoyo_i [] matr1 matr2 (i-1) (j-1)

-- цикл по строкам
yoyo_i :: [[Double]] -> [[Double]] -> [[Double]] -> Int -> Int -> [[Double]]
yoyo_i res matr1 matr2 i j
  | (i == -1) = res
  | otherwise = yoyo_i ((yoyo_j [] matr1 matr2 i j):res) matr1 matr2 (i-1) j

-- цикл по столбцам
yoyo_j :: [Double] -> [[Double]] -> [[Double]] -> Int -> Int -> [Double]
yoyo_j res matr1 matr2 i j
  | (j == -1) = res
  | otherwise = yoyo_j (((get_elem matr1 i j) + (get_elem matr2 i j)):res) matr1 matr2 i (j-1)

-- получение элемента матрицы
get_elem :: [[a]] -> Int -> Int -> a
get_elem matr i j = result
  where
    list = get_l matr i
    result = get_l list j

-- получения элемента списка
get_l :: [a] -> Int -> a
get_l (h:t) i
  | (i == 0) = h
  | otherwise = get_l t (i-1)

main :: IO ()
main = print(summ 4 3 a b) where 
  a = [[1,2,3],[5,6,7],[9,1,2],[4,5,6]]
  b = [[8,9,1],[3,4,5],[7,8,9],[2,3,4]]