-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)


type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


--modelar un palo de golf
{-

data Obstaculo = UnObstaculo {
  nombreObstaculo :: String,
  superable :: (Tiro -> Bool)
} deriving (Eq, Show)
-}

--Punto 1
data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro {
  velocidad = 10,
  precision = precisionJugador habilidad * 2,
  altura = 0
}


golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)