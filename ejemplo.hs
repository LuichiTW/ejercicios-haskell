--comentario

--ejemplo de definicion de una funcion
doble :: Int -> Int
doble numero = numero * 2

siguiente :: Int -> Int
siguiente numero = numero + 1

suma :: Int -> Int -> Int
suma unNumero otroNumero = unNumero + otroNumero

--ejemplo de composicion de funciones
dobleDelSiguiente :: Int -> Int
dobleDelSiguiente = doble.siguiente


--ejemplo de aplicacion parcial
esMayorDeEdad :: Int -> Bool
esMayorDeEdad edad = edad >= 18

frecuanciaCardiacaPromedio :: Int 
frecuanciaCardiacaPromedio = 80

hacerActividadFisica:: Int -> Int
hacerActividadFisica unaFrecuencia = unaFrecuencia  + 50

tieneTaquicardia :: Int -> Bool
tieneTaquicardia frecuencia = frecuencia >= 100

--ejemplo familia de tipos
sumaReal :: Num a => a -> a -> a
sumaReal unNumero otroNumero = unNumero + otroNumero

--ejemplo de tuplas (String, Int, String)
primerElemento :: (String, Int, String) -> String
primerElemento (primero, _, _) = primero

--ejemplo de etiquetas de campos
type Libro = (String, String, Int)

elVisitante :: Libro
elVisitante = ("el visitante", "Stephen King", 592)

shingekiNoKyojin1 :: Libro
shingekiNoKyojin1 = ("shingeki no kyojin 1 ", "Hajime Isayama", 40)


esVocal :: Char -> Bool
esVocal unCaracter = elem unCaracter "aeiouAEIOUÁÉÍÓÚ"

--ejemplo de pattern matching
esLecturaObligatoria' :: Libro -> Bool
esLecturaObligatoria' (_, "Stephen King", _) = True
esLecturaObligatoria' ("Fundacion", "Isaac Asimov", 230) = True

--ejemplo de guardas
esLecturaObligatoria :: Libro -> Bool
esLecturaObligatoria libro
    | esLibroDeStephenKing libro = True
    | esFundacion libro = True
    | otherwise = False

esLibroDeStephenKing :: Libro -> Bool
esLibroDeStephenKing (_, "Stephen King", _) = True

esFundacion :: Libro -> Bool
esFundacion ("Fundacion", _, _) = True

{-ejemplo de que no se hace con guardas

esLecturaObligatoria :: Libro -> Bool 
esLecturaObligatoria unLibro
            | unLibro == eragon = True
            | unLibro == eldest = True
            | unLibro == brisignr = True
            | unLibro == legado = True
            | autor unLibro == "Stephen King" = True
            | unLibro == fundacion = True
	|otherwise = False

en este ejemplo falta delegar la responsabilidad de la comparacion a una funcion
-}



data Libro' = UnLibro {
    titulo :: String,
    autor :: String,
    paginas :: Int
} deriving (Show, Eq)


agregarPaginas :: Libro' -> Int -> Libro'
agregarPaginas unLibro paginasAAgregar = unLibro { paginas = paginas unLibro +  paginasAAgregar}


dobles :: Num a => [a] -> [a]
dobles numeros = map (\numero -> numero * 2) numeros

{-
    cuando se uso lambda:

    Cuando no tenemos un buen nombre para ponerle a una función.
    Si sólo la vamos a usar una única vez, sino estamos repitiendo lógica.
    Si no necesitamos usar guardas ni pattern matching (con más de una ecuación).

-}

--ejemplo de currificacion
sumaDe3Numeros' :: (Int -> Int -> Int -> Int)
sumaDe3Numeros' = (\numero1 numero2 numero3 -> numero1 + numero2 + numero3)

sumaDe3Numeros'' :: (Int -> (Int -> Int -> Int))
sumaDe3Numeros'' = (\numero1 -> (\numero2 numero3 -> numero1 + numero2 + numero3))


--ejemplo de recursividad
factorial :: Int -> Int
factorial 0 = 1                     -- caso base
factorial n = n * factorial (n - 1) -- caso recursivo

fibonacci :: Int -> Int
fibonacci 0 = 0                                      -- caso base
fibonacci 1 = 1                                      -- caso base
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)  -- caso recursivo


--ejemplo de lazy evaluation
infinito :: [Int]
infinito = [1..]

primeros10 :: [Int]
primeros10 = take 10 infinito


sumaRecursiva :: [Int] -> Int
sumaRecursiva [] = 0
sumaRecursiva (x:xs) = x + sumaRecursiva xs