import Text.Show.Functions

data Personaje = UnPersonaje {
    nombre :: String,
    poderBasico :: Poder,
    superPoder :: Poder,
    superActivo :: Bool,
    vida :: Int
} deriving Show

type Poder = Personaje -> Personaje

espina :: Personaje
espina = UnPersonaje {
    nombre = "Espina",
    poderBasico = bolaEspinosa,
    superPoder = granadaDeEspinas 5,
    superActivo = True,
    vida = 4800
}

pamela :: Personaje
pamela = UnPersonaje {
    nombre = "Pamela",
    poderBasico = lluviaDeTuercas "sanadora",
    superPoder = torretaCurativa,
    superActivo = False,
    vida = 9600
}

brawlers :: [Personaje]
brawlers = [espina, pamela]

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa personaje = personaje { vida = modificarVida (-) 1000.vida$personaje}

modificarVida :: (Int -> Int -> Int) -> Int -> Int -> Int
modificarVida funcion valor vida
    |funcion vida valor < 0 = 0
    |otherwise = funcion vida valor

granadaDeEspinas :: Int -> Personaje -> Personaje
granadaDeEspinas radio personaje
    | radio > 3 = personaje {nombre = "espina estuvo aqui", vida = ejecucion.vida$personaje, superActivo = (==0).ejecucion.vida$personaje}
    | otherwise = bolaEspinosa personaje

ejecucion :: Int -> Int
ejecucion vida
    | vida < 800 = 0
    | otherwise = modificarVida (-) 1000 vida 

lluviaDeTuercas :: String -> Personaje -> Personaje
lluviaDeTuercas tipo personaje
    | tipo == "sanadora" = personaje  { vida = modificarVida (+) 800.vida$personaje}
    | tipo == "danina" = personaje { vida = modificarVida (-) (div (vida personaje) 2).vida$personaje}
    | otherwise = personaje

torretaCurativa :: Personaje -> Personaje
torretaCurativa personaje = personaje { vida = modificarVida (+) (vida personaje * 2).vida$personaje, superActivo = True}

atacarConSuperActivo :: Personaje -> Personaje -> Personaje
atacarConSuperActivo personajeAtacante personajeContrincante
    | superActivo personajeAtacante = superPoder personajeAtacante.poderBasico personajeAtacante$personajeContrincante
    | otherwise = personajeContrincante

estaEnLasUltima :: [Personaje] -> [String]
estaEnLasUltima personajes = map nombre personajes

filtrarEnLasUltima :: [Personaje] -> [Personaje]
filtrarEnLasUltima personajes = filter ((<800).vida) personajes