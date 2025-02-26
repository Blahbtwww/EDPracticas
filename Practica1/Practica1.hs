-- Practica1.hs

-- 1. Distancia entre dos puntos en el plano cartesiano.
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) =
  sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- 2. Valor absoluto de un número.
valorAbsoluto :: Int -> Int
valorAbsoluto n =
  if n >= 0
    then n
    else -n

-- 3. Pendiente de la recta que pasa por dos puntos.
pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente (x1, y1) (x2, y2) =
  (y2 - y1) / (x2 - x1)

-- 4. Hipotenusa de un triangulo rectángulo.
hipotenusa :: Float -> Float -> Float
hipotenusa b h =
  sqrt (b^2 + h^2)

-- 5. Raíces de una ecuación cuadrática.
raices :: Float -> Float -> Float -> (Float, Float)
raices a b c =
  ( (-b + sqrt (b^2 - 4*a*c)) / (2*a)
  , (-b - sqrt (b^2 - 4*a*c)) / (2*a)
  )

-- 6. Área de un triángulo por medio de la fórmula Herón.
areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo x y z =
  sqrt ( (x + y + z) / 2
       * ((x + y + z) / 2 - x)
       * ((x + y + z) / 2 - y)
       * ((x + y + z) / 2 - z)
       )

-- 7. Determinar si un año es bisiesto.
esBisiesto :: Int -> Bool
esBisiesto year =
  if year `mod` 400 == 0 then
    True
  else
    if year `mod` 100 == 0 then
      False
    else
      if year `mod` 4 == 0 then
        True
      else
        False

-- 8. Función comparador.
comparador :: Int -> Int -> Int
comparador x y =
  if x == y then
    0
  else
    if x < y then
      -1
    else
      1

-- 9. Máximo entre tres números.
maximo :: Int -> Int -> Int -> Int
maximo x y z =
  if x >= y && x >= z then
    x
  else
    if y >= x && y >= z then
      y
    else
      z

-- 10. Verifica si el orden esta de forma descendente.
esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente x y z w =
  x > y && y > z && z > w
