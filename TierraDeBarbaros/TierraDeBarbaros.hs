import Text.Show.Functions()
import Data.Char (toUpper)



data Barbaro = UnBarbaro {
    nombre :: String,
    fuerza :: Int,
    habilidades :: [String],
    objetos :: [Objeto]
} deriving Show

type Objeto = Barbaro -> Barbaro

dave :: Barbaro
dave = UnBarbaro {
    nombre = "Dave",
    fuerza = 100,
    habilidades = ["tejer", "escribirPoesia"],
    objetos = [ardilla, varitaDefectuosa]
}

faffy :: Barbaro
faffy = UnBarbaro {
    nombre = "Faffy",
    fuerza = 1,
    habilidades = ["tejer", "escribirPoesia"],
    objetos = [ardilla, varitaDefectuosa]
}

astro :: Barbaro
astro = UnBarbaro {
    nombre = "Astro",
    fuerza = 60,
    habilidades = ["tejer", "escribirPoesia"],
    objetos = [ardilla, varitaDefectuosa]
}

-- punto 1

espada :: Int -> Barbaro -> Barbaro
espada peso barbaro = barbaro {fuerza = fuerza barbaro + 2*peso}

ardilla :: Barbaro -> Barbaro
ardilla barbaro = barbaro

amuletoMistico :: Barbaro -> Barbaro
amuletoMistico barbaro = barbaro

varitaDefectuosa :: Barbaro -> Barbaro
varitaDefectuosa barbaro = barbaro {objetos = [], habilidades = ["magia"] ++ habilidades barbaro  }

cuerda :: Objeto -> Objeto -> Objeto
cuerda objeto1 objeto2 = objeto1.objeto2

-- punto 2

megafono :: Barbaro -> Barbaro
megafono barbaro = ponerEnMayuscula.concatenar$barbaro

concatenar :: Barbaro -> Barbaro
concatenar barbaro = barbaro {habilidades = [foldr (++) "" (habilidades barbaro)]}

ponerEnMayuscula :: Barbaro -> Barbaro
ponerEnMayuscula barbaro = barbaro {habilidades = [map toUpper (head (habilidades barbaro))]}

megafonoBarbarico :: Barbaro -> Barbaro
megafonoBarbarico barbaro = cuerda ardilla megafono$barbaro

-- punto 3

invasionDeSuciosDuendes :: Barbaro -> Bool
invasionDeSuciosDuendes barbaro = elem "Escribir Poesía Atroz" (habilidades barbaro)

cremalleraDelTiempo :: Barbaro -> Bool
cremalleraDelTiempo barbaro = not.tienePulgares$nombre barbaro

tienePulgares :: String -> Bool
tienePulgares nombre
    | nombre == "Faffy" = False
    | nombre == "Astro" = False
    | otherwise = True

ritualDeFechorias :: Barbaro -> Bool
ritualDeFechorias barbaro
    | saqueo barbaro = True
    | gritoDeGuerra barbaro = True
    | caligrafia barbaro = True
    | otherwise = False

saqueo :: Barbaro -> Bool
saqueo barbaro = elem "saqueo" (habilidades barbaro) && fuerza barbaro > 80

gritoDeGuerra :: Barbaro -> Bool
gritoDeGuerra barbaro = ((4*).length.objetos$barbaro) < (length.concat.habilidades$barbaro)

caligrafia :: Barbaro -> Bool
caligrafia barbaro = (all contiene3Vocales.habilidades$barbaro) && comienzaConMayuscula (head.habilidades$barbaro)

contiene3Vocales :: String -> Bool
contiene3Vocales palabra = length (filter (`elem` "aeiou") palabra) >= 3

comienzaConMayuscula :: String -> Bool
comienzaConMayuscula palabra = elem (head palabra) ['A'..'Z']

sobrevivientes :: [Barbaro] -> (Barbaro -> Bool) -> [Bool]
sobrevivientes lista aventura = map aventura lista

-- punto 4

sinRepetidos :: [String] -> [String]
sinRepetidos [] = []
sinRepetidos lista = listaSinRepetido (head lista) (sinRepetidos (tail lista))

listaSinRepetido :: String -> [String] -> [String]
listaSinRepetido str lista =  [str] ++ filter (/=str) lista 

descendientes :: Barbaro -> Barbaro
descendientes barbaro = barbaro {habilidades = sinRepetidos (habilidades barbaro)}

modificarNombre :: (String -> String -> String) -> String -> Barbaro -> Barbaro
modificarNombre funcion agregado barbaro = barbaro {nombre = funcion (nombre barbaro) agregado}