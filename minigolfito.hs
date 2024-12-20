{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

import Text.Show.Functions
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

--Punto 1
{-
Sabemos que cada palo genera un efecto diferente, por lo tanto elegir el palo correcto puede ser la diferencia entre ganar o perder el torneo.
Modelar los palos usados en el juego que a partir de una determinada habilidad generan un tiro que se compone por velocidad, precisión y altura.
+El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.
+La madera genera uno de velocidad igual a 101, altura igual a 5 y la mitad de la precisión.
+Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de velocidad igual a la fuerza multiplicada por n, la precisión dividida por n y una altura de n-3 (con mínimo 0). Modelarlos de la forma más genérica posible.

Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego.

-}
data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro {
  velocidad = 10,
  precision = (*2).precisionJugador $ habilidad,
  altura = 0
}

madera :: Palo
madera habilidad = UnTiro {
  velocidad = 100,
  precision = div (precisionJugador habilidad) 2,
  altura = 5
}

hierro :: Int -> Palo
hierro n habilidad = UnTiro {
  velocidad = fuerzaJugador habilidad * n,
  precision = div (precisionJugador habilidad) n,
  altura = max (n-3) 0
}

palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]

--Punto 2
{-
Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese palo con las habilidades de la persona.
Por ejemplo si Bart usa un putter, se genera un tiro de velocidad = 10, precisión = 120 y altura = 0.
-}

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo.habilidad $ jugador

--Punto 3
{-
Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, y en el caso de poder superarlo, cómo se ve afectado dicho tiro por el obstáculo. En principio necesitamos representar los siguientes obstáculos:

+Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, independientemente de la velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0.

+Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. Luego de superar una laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura original dividida por el largo de la laguna.

+Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.

Se desea saber cómo queda un tiro luego de intentar superar un obstáculo, teniendo en cuenta que en caso de no superarlo, se detiene, quedando con todos sus componentes en 0.

-}

tiroNulo :: Tiro
tiroNulo = UnTiro {velocidad = 0, precision = 0, altura = 0}
 
data Obstaculo = Obstaculo{
requisitosObstaculo :: Tiro -> Bool,
efectoObstaculo :: Tiro -> Tiro
} deriving(Show)

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo unObstaculo unTiro 
  |requisitosObstaculo unObstaculo unTiro = efectoObstaculo unObstaculo unTiro
  |otherwise = tiroNulo
  
tunel :: Obstaculo
tunel = Obstaculo{requisitosObstaculo = requisitoTunel, efectoObstaculo= efectoTunel}

requisitoTunel :: Tiro -> Bool
requisitoTunel unTiro = ((>90).precision $ unTiro) && ((==0).altura $ unTiro) 
efectoTunel :: Tiro -> Tiro
efectoTunel unTiro = UnTiro{velocidad= velocidad unTiro * 2, precision = 100, altura = 0}

between n m x = elem x [n .. m]

laguna :: Int -> Obstaculo
laguna largo = Obstaculo {requisitosObstaculo = requisitoLaguna, efectoObstaculo = efectoLaguna largo}

requisitoLaguna :: Tiro -> Bool
requisitoLaguna unTiro = ((>80).velocidad $ unTiro) && (between 1 5 (altura unTiro)) 
efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largo unTiro = UnTiro {velocidad = velocidad unTiro,precision = precision unTiro, altura = div (altura unTiro) largo}


hoyo :: Obstaculo
hoyo = Obstaculo {requisitosObstaculo = requisitosHoyo ,efectoObstaculo = efectoHoyo}

--Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.
requisitosHoyo :: Tiro -> Bool
requisitosHoyo unTiro = between 5 20 (velocidad unTiro) && precision unTiro > 95 && vaAlRasDelSuelo unTiro
efectoHoyo :: Tiro -> Tiro
efectoHoyo unTiro = tiroNulo

vaAlRasDelSuelo :: Tiro -> Bool
vaAlRasDelSuelo unTiro = (==0).altura $ unTiro


--Punto 4
{-
+Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.

+Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden superar.
Por ejemplo, para un tiro de velocidad = 10, precisión = 95 y altura = 0, y una lista con dos túneles con rampita seguidos de un hoyo, el resultado sería 2 ya que la velocidad al salir del segundo túnel es de 40, por ende no supera el hoyo.
BONUS: resolver este problema sin recursividad, teniendo en cuenta que existe una función takeWhile :: (a -> Bool) -> [a] -> [a] que podría ser de utilidad.

+Definir paloMasUtil que recibe una persona y una lista de obstáculos y determina cuál es el palo que le permite superar más obstáculos con un solo tiro.

-}
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = filter (puedeSuperar unJugador unObstaculo) palos

puedeSuperar :: Jugador -> Obstaculo -> Palo -> Bool
puedeSuperar unJugador unObstaculo unPalo = requisitosObstaculo unObstaculo (golpe unJugador unPalo)

obstaculosConsecutivos :: [Obstaculo] -> Tiro -> Int
obstaculosConsecutivos [] _ = 0
obstaculosConsecutivos (x:xs) unTiro
  |requisitosObstaculo x unTiro = 1 + obstaculosConsecutivos xs (efectoObstaculo x unTiro)
  |otherwise = 0

tiro1 :: Tiro
tiro1 = UnTiro {
  velocidad = 10,
  precision = 95,
  altura = 0
}

maximoSegun :: Ord a1 => (a2 -> a1) -> [a2] -> a2
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord a => (t -> a) -> (t -> t -> t)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil unJugador unosObstaculos = maximoSegun (obstaculosConsecutivos unosObstaculos.golpe unJugador) palos
