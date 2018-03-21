-- Versão mais abrangente da função isInMandelbrot, onde toma como argumento uma função que faz uma operação com dois números complexos
-- (z e c) um valor inicial para z e um valor fixo para c (todos os números complexos sendo em forma de uma tuple dupla, representando a 
-- parte real e imaginária)

isInJuliaSet :: (RealFloat a) => ((a, a) -> (a, a) -> (a, a)) -> (a, a) -> Bool
isInJuliaSet f z c = (complexMag . setIter f z c $ steps) < 2
  where complexMag (x, y) = sqrt $ x**2 + y**2
        steps = 50
        setIter :: (RealFloat a, Integral b) => (a, a) -> (a, a) -> b -> (a, a)
        setIter f z c step
          | step <= 0 = f z c
          | otherwise = setIter (f z c) c (step - 1)
		  

-- Exemplo 1: o equivalente de isInMandelbrot, utilizando esta função, seria:

isInMandelbrot = isInJuliaSet zSquaredPlusC (0.0, 0.0)
	where zSquaredPlusC z c = (complexSq z) `complexAdd` c
		  complexSq (x, y) = (x**2 - y**2, 2*x*y)
		  complexAdd (x1, y1) (x2, y2) = (x1+x2, y1+y2)
		  
		  
-- Exemplo 2: o equivalente a um Conjunto de Julia, cuja iteração é z = sin(z)*c, e o c inicial é 1 + 0.5i será (nos conjuntos de Julia,
-- em contrapartida ao de Mandelbrot, o z depende do local escolhido, e o c apenas define o padrão:

isInSinJuliaSet z = isInJuliaSet sinZTimesC z (1, 0.5)
	where sinZTimesC z c = (complexSin z) `complexTimes` c
		  -- sin(z) = (e^iz - e^-iz) / 2i
		  complexSin z = let iz = (0, 1) `complexTimes` z in (exp(iz) - exp(-iz)) `complexTimes` (0, 1/2)
		  complexTimes (x1, y1) (x2, y2) = (x1*x2 - y1*y2, x1*y2 + y1*x2)
