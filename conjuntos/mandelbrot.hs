-- A função toma como argumento um tuple de dois numeros, que representam, respectivamente, a parte real e a parte imaginária do número
-- e retorna um valor booleano, dizendo se ele pertence ou não ao conjunto de Mandelbrot

isInMandelbrot :: (RealFloat a) => (a, a) -> Bool
isInMandelbrot (a, b) = (complexMag . mandelbrotIter (0.0, 0.0) (a, b) $ steps) < 2
  where complexMag (x, y) = sqrt $ x**2 + y**2
        steps = 50
        mandelbrotIter :: (RealFloat a, Integral b) => (a, a) -> (a, a) -> b -> (a, a)
        mandelbrotIter z@(z1, z2) c@(c1, c2) step
          | step <= 0 = newZ z c
          | otherwise = mandelbrotIter (newZ z c) c (step - 1)
          where newZ a b = (complexSq a) `complexAdd` b
                complexSq (x, y) = (x**2 - y**2, 2*x*y)
                complexAdd (x1, y1) (x2, y2) = (x1+x2, y1+y2)
