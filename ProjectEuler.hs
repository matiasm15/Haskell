
-- Devuelve True si el primero numero divide al segundo.
esDivisor dividendo divisor = (mod dividendo divisor == 0)

-- Devuelve True si el numero es primo.
esPrimo numero = length(factores numero) == 2

-- Devuelve una lista infinita con los terminos de la sucesión de Fibonacci dados los dos terminos iniciales.
fibonacciSem primerElem segundoElem = segundoElem:(fibonacciSem segundoElem (primerElem + segundoElem))

-- Dada una lista, devuelve la lista habiendo eliminado los elementos repetidos.
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | elem x xs = eliminarRepetidos xs
                         | otherwise = x:(eliminarRepetidos xs)

-- Devuelve una lista con los factores de un numero dado.
factores numero = [factor | factor <- [1..numero], esDivisor numero factor]

factores2 numero = lFactoresHastaRaiz ++ map (div numero) lFactoresHastaRaiz
	where lFactoresHastaRaiz = factoresHastaRaiz numero

factoresHastaRaiz numero = [factor | factor <- [1..((floor.sqrt) numero)], (mod numero factor == 0)]

--1)
ejercicio1 limite = sum [numero | numero <- [1..(limite - 1)], (esDivisor numero 3) || (esDivisor numero 5)]

--2)
ejercicio2 limite = sum (filter even (fibonacciMenoresA limite))

fibonacciMenoresA limite = takeWhile (limite >) (fibonacciSem 0 1)

--3)
ejercicio3 limite = last (filter esPrimo (factores limite))

--448)
ejercicio448 numero = sum(map funcionA [1..numero])
	where funcionA numero = div (sum(map (lcm numero) [1..numero])) numero
