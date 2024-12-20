import Text.Show.Functions()
import Data.List()

data Auto = UnAuto {
    color :: String,
    velocidad :: Int,
    distancia :: Int
} deriving (Show, Eq, Ord)

type Carrera = [Auto]

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = sonDiferentes auto1 auto2 && abs (distancia auto1 - distancia auto2) < 10

sonDiferentes :: Auto -> Auto -> Bool
sonDiferentes auto1 auto2 = auto1 /= auto2

vaTranquilo :: Carrera -> Auto -> Bool
vaTranquilo carrera auto = all (not.estaCerca auto) carrera && vaPrimero carrera auto

vaPrimero :: Carrera -> Auto -> Bool
vaPrimero carrera auto = (==1).quePuestoOcupa carrera $ auto

quePuestoOcupa :: Carrera -> Auto -> Int
quePuestoOcupa carrera auto = length (filter ((distancia auto<).distancia) carrera) + 1

correr :: Int -> Auto -> Auto
correr tiempo auto = auto {distancia = distancia auto + velocidad auto * tiempo}

alterarVelocidad :: (Int->Int) -> Auto -> Auto
alterarVelocidad modificador auto = auto {velocidad = modificador (velocidad auto)}

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad cantidad auto
    |velocidad auto - cantidad < 0 = alterarVelocidad (const 0) auto
    |otherwise = alterarVelocidad (subtract cantidad) auto

type PowerUp = Carrera -> Carrera

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: Auto -> PowerUp
terremoto auto carrera = afectarALosQueCumplen (estaCerca auto) (bajarVelocidad 50) carrera

miguelitos :: Int -> Auto -> PowerUp
miguelitos cantidad auto carrera = afectarALosQueCumplen ((distancia auto <).distancia) (bajarVelocidad cantidad) carrera

jetPack :: Int -> Auto -> PowerUp
jetPack tiempo auto carrera = afectarALosQueCumplen (auto==) (alterarVelocidad (flip div 2).(correr tiempo).alterarVelocidad (*2)) carrera

type Color = String

type TablaDePosiciones = [(Int, Color)]

simularCarrera :: Carrera -> [PowerUp] -> TablaDePosiciones
simularCarrera carrera powerUps = crearTablaDePosiciones.foldr ($) carrera $ powerUps 

--(foldr ($) carrera powerUps)

crearTablaDePosiciones :: Carrera -> TablaDePosiciones
crearTablaDePosiciones carrera = zip [1..] (map color (reOrdenarPorPuesto carrera))

reOrdenarPorPuesto :: Carrera -> Carrera
reOrdenarPorPuesto [] = []
reOrdenarPorPuesto carrera = obtenerAutos vaPrimero carrera ++ reOrdenarPorPuesto (obtenerAutosQueNocumplen vaPrimero carrera)

obtenerAutos :: (Carrera -> Auto -> Bool) -> Carrera -> Carrera
obtenerAutos funcion carrera = filter (funcion carrera) carrera

obtenerAutosQueNocumplen :: (Carrera -> Auto -> Bool) -> Carrera -> Carrera
obtenerAutosQueNocumplen funcion carrera = filter (not.funcion carrera) carrera

azul :: Auto
azul = UnAuto "azul" 120 0

rojo :: Auto
rojo = UnAuto "rojo" 120 0

blanco :: Auto
blanco = UnAuto "blanco" 120 0

negro :: Auto
negro = UnAuto "negro" 120 0

carrera1 :: Carrera
carrera1 = [rojo, azul, blanco, negro]

correnTodos :: Int -> Carrera -> Carrera
correnTodos tiempo carrera = map (correr tiempo) carrera

usaPowerUp :: (Auto -> Carrera -> Carrera) -> String -> PowerUp
usaPowerUp powerUp color carrera = powerUp (buscarAuto color carrera) carrera

buscarAuto :: String -> Carrera -> Auto
buscarAuto unColor carrera = head.filter ((==unColor).color) $ carrera

eventos :: [PowerUp]
eventos = [correnTodos 30, jetPack 3 azul, terremoto blanco, correnTodos 40, miguelitos 20 blanco, jetPack 6 negro, correnTodos 10]

--simularCarrera carrera1 eventos


{-
a-La solucion permite agregar la funcion sin cambiar ningun punto

b- 
-}