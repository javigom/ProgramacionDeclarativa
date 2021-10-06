-- JAVIER GÓMEZ MORALEDA

-- 1)

-- ESTRUCTURA DE DATOS DE LA RELACIÓN --
data Rel a = Rel [(a,a)] deriving(Read, Show)

-- Ejemplo 1
r1 :: Rel Char
r1 = Rel [('D', '8'), ('E', 'B'), ('C', 'B'), ('E', 'C'), ('8', 'D')]

-- Ejemplo 2
r2 :: Rel [Int]
r2 = Rel [([1,2], []), ([2, -2], [3, 3, 1]), ([1,3], [0]), ([4], [4])]

-- TRUE SI ES UNA RELACIÓN -- 
esRelacion :: Eq a => Rel a -> Bool
esRelacion (Rel rs) = sinRepetidos(rs)

-- Función auxiliar definida de manera recursiva. Será evaluada a True si una lista no tiene elementos repetidos
sinRepetidos :: Eq a => [a] -> Bool
sinRepetidos [] = True
sinRepetidos [x] = True
sinRepetidos (x:xs)
 | elem x xs = False
 | otherwise = sinRepetidos xs

-- Ejemplo para comprobar que no es una relación
r3 :: Rel Int
r3 = Rel [(2,2), (3,2), (3,3), (3,2)]


-- 2)

-- REDEFINICIÓN DEL MÉTODO DE LA CLASE IGUALDAD --
instance Eq a => Eq (Rel a) where
    (Rel xs) == (Rel ys) = foldr (&&) True [elem x ys | x <- xs] && (longitudRelacion (Rel xs) == longitudRelacion (Rel ys))

-- True si todos los elementos de una relación se encuentran en otra
comparaRelaciones :: Eq a => Rel a -> Rel a -> Bool
comparaRelaciones (Rel xs) (Rel ys) = foldr (&&) True [elem x ys | x <- xs] && (longitudRelacion (Rel xs) == longitudRelacion (Rel ys))

-- Obtiene el número de pares en una relación
longitudRelacion :: Rel a -> Int
longitudRelacion (Rel xs) = sum [1 | x <- xs]

-- Ejemplos para comprobar que contienen los mismos conjuntos (aunque en distinto orden)
r4, r5 :: Rel Int
r4 = Rel[(1, 2), (2, 2)]
r5 = Rel[(2, 2), (1, 2)]


-- 3)

-- CONJUNTO DOMINIO DE LA RELACIÓN --
dominio :: Eq a => Rel a -> [a]
dominio (Rel xr) 
 | esRelacion (Rel xr) == False = error "No es una relación"
 | otherwise = eliminarRepetidos [x | (x, y) <- xr]

-- Elimina los elementos repetidos del dominio
eliminarRepetidos :: Eq a => [a] -> [a]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) 
 | elem x xs = eliminarRepetidos xs
 | otherwise = x : eliminarRepetidos xs

-- CONJUNTO SOPORTE DE LA RELACIÓN --
soporte :: Eq a => Rel a -> [a]
soporte (Rel xr) 
 | esRelacion (Rel xr) == False = error "No es una relación"
 | otherwise = eliminarRepetidos ([x | (x, y) <- xr] ++ [y | (x, y) <- xr])

-- TRUE SI SE TRATA DE UNA RELACIÓN DE EQUIVALENCIA --
relEquivalencia :: Eq a => Rel a -> Bool
relEquivalencia (Rel xr) 
 | esRelacion (Rel xr) == False = error "No es una relación"
 | otherwise = (esReflexiva (Rel xr)) && (esSimetrica (Rel xr)) && (esTransitiva (Rel xr))

-- True si la relación es reflexiva, es decir, que para todo x en la relación, existe el par (x, x)
esReflexiva :: Eq a => Rel a -> Bool
esReflexiva (Rel xr) = foldr (&&) True [elem (y, y) xr | y <- ys] 
 where ys = soporte (Rel xr)

-- True si la relación es simétrica, es decir, que para todo x, y de la relación, si existe el par (x, y) entonces existe el par (y, x)
esSimetrica :: Eq a => Rel a -> Bool
esSimetrica (Rel xr) = foldr (&&) True [elem (y, x) xr | (x, y) <- xr]

-- True si la relación es transitiva, es decir, que para todo x, y, z de la relación, si existen los pares (x, y), (y, z), entonces existe el par (x, z)
esTransitiva :: Eq a => Rel a -> Bool
esTransitiva (Rel xr) = foldr (&&) True [esTransitivaAux1 (Rel xr) x | x <- xr]

-- Función auxiliar que es True si dado un par (x1, y1), si en la relación existe un par (y1, z), entonces existe el par (x1, z) en la relación
esTransitivaAux1 :: Eq a => Rel a -> (a, a) -> Bool
esTransitivaAux1 (Rel xr) (x1, y1) = esTransitivaAux2 (Rel xr) ([(x1, z) | (y, z) <- xr, y == y1])

-- Función auxiliar que es True si todos los pares de ys se encuentran en la relación
esTransitivaAux2 :: Eq a => Rel a -> [(a, a)] -> Bool
esTransitivaAux2 (Rel xr) ys = foldr (&&) True [elem y xr | y <- ys]

-- Ejemplo para probar la relación de equivalencia
r6 :: Rel Int
r6 = Rel[(1, 2), (2, 3), (1, 1), (2, 2), (1, 3), (2, 1), (3, 2), (3, 1), (3, 3), (4, 4)]

--CONJUNTO COCIENTE DE UNA RELACIÓN --
conjCociente :: Ord a => Rel a -> [[a]]
conjCociente (Rel xr) 
 | esRelacion (Rel xr) == False = error "No es una relación"
 | relEquivalencia (Rel xr) == False = error "La relación debe ser de equivalencia" 
 | otherwise = eliminarRepetidos [conjCocienteAux (Rel xr) x | (x, y) <- xr]

-- Función auxiliar donde se obtiene la clase de equivalencia de z ordenadas por elementos
conjCocienteAux (Rel xr) z = quickSort [y | (x, y) <- xr, x == z]

-- Algoritmo de ordenación para los distintas clases de equivalencia
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort(menores) ++ [x] ++ quickSort(mayores)
 where
 menores = [y | y <-xs, y < x]
 mayores = [z | z <-xs, z >= x]

-- CREA UNA RELACION DE PARES DIVISORES (EXISTE (X, Y) SI X ES DIVISOR DE Y, Y ADEMÁS, N <= X E Y <= M) --
generaDiv :: Integral a => a -> a -> Rel a
generaDiv n m = Rel [(x, y) | x <- [n..m], y <- [n..m], x <= y, (rem y x) == 0]

-- CREA LA RELACIÓN >= SOBRE LOS ELEMENTOS DE XS --
generaGE :: Ord a => [a] -> Rel a
generaGE xs = Rel [(x, y) | x <- xs, y <- xs, x >= y]

-- COMPOSICIÓN DE LAS RELACIONES R1 Y R2 -- 
composicion :: Ord a => Eq a => Rel a -> Rel a -> Rel a
composicion (Rel xr) (Rel yr) 
 | esRelacion (Rel xr) == False || esRelacion (Rel yr) == False = error "Alguno de los dos argumentos no es una relación"
 | quickSort (soporte (Rel xr)) /= quickSort (soporte (Rel yr)) = error "Las relaciones no están definidas sobre el mismo conjunto soporte"
 | otherwise = Rel $ foldr (++) [] [composicionAux (Rel yr) x | x <- xr]

-- Pares con el elemento y1 de la relacion en yr
composicionAux :: Eq a => Rel a -> (a, a) -> [(a, a)]
composicionAux (Rel yr) (x1, y1) = [(x1, z) | (y, z) <- yr, y == y1]

-- Ejemplos para probar la composición de dos relaciones
r7, r8 :: Rel Int
r7 = Rel[(1,1), (2,3), (4,5)]
r8 = Rel[(1,2), (3,5), (5,4)]


-- 4)

-- LEE UNA RELACIÓN INTRODUCIDA POR EL USUARIO --
introRel :: IO (Rel Int)
introRel = introRelAux (Rel [])

-- Introduce los pares que quiera el usuario en la relación
introRelAux :: Rel Int -> IO (Rel Int)
introRelAux r = do
    putStrLn "Introduce el primer elemento: "
    e1 <- getLine
    e1 <- return (read e1::Int) 

    putStrLn "Introduce el segundo elemento: "
    e2 <- getLine
    e2 <- return (read e2::Int)

    putStrLn "Seguir introduciendo tuplas? [s/n]: "
    res <- getLine

    let r1 = anyadePar r (e1, e2)

    if res == "s" then do 
        introRelAux r1
    else return r1

-- Añade un nuevo par a la relación
anyadePar :: Rel a -> (a, a) -> Rel a
anyadePar (Rel xr) x = Rel y where y = (x:xr)

muestraRel :: IO()
muestraRel = do
    r <- introRel