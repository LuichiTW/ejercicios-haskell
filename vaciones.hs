import Text.Show.Functions
{-
Una empresa de turismo localizada en una isla que nadie sabe dónde está nos pide construir un software que los ayude a sacar estadísticas de los tours que ofrece a sus clientes: los turistas…
De cada turista nos interesa:
Sus niveles de cansancio y stress
Si está viajando solo
Los idiomas que habla
-}
data Turista = UnTurista {
    nivelCansancio :: Int,
    nivelEstres :: Int,
    estaSolo :: Bool,
    idiomas :: [String]
} deriving (Eq, Show)

{-
Ir a la playa: si está viajando solo baja el cansancio en 5 unidades, si no baja el stress 1 unidad.
-}
irALaPlaya :: Turista -> Turista
irALaPlaya unTurista
    |estaSolo unTurista= unTurista {nivelCansancio= nivelCansancio unTurista - 5} 
    |otherwise = unTurista {nivelEstres= nivelEstres unTurista - 1}

{-
Apreciar algún elemento del paisaje: reduce el stress en la cantidad de letras de lo que se aprecia. 
-}
apreciarPaisaje :: String -> Turista -> Turista
apreciarPaisaje unPaisaje unTurista = unTurista {nivelEstres = nivelEstres unTurista - length unPaisaje}

{-
Salir a hablar un idioma específico: el turista termina aprendiendo dicho idioma y continúa el viaje acompañado.
-}
hablarIdioma :: String -> Turista -> Turista 
hablarIdioma idioma unTurista = unTurista {estaSolo = False, idiomas = aprenderIdioma idioma unTurista} 

aprenderIdioma :: String -> Turista -> [String]
aprenderIdioma idioma unTurista = [idioma] ++ idiomas unTurista

{-
Caminar ciertos minutos: aumenta el cansancio pero reduce el stress según la intensidad de la caminad, ambos en la misma cantidad. El nivel de intensidad se calcula en 1 unidad cada 4 minutos que se caminen.
-}
caminar :: Int -> Turista -> Turista
caminar minutos unTurista = unTurista {nivelCansancio = nivelCansancio unTurista + calcularIntensidad minutos, nivelEstres = nivelEstres unTurista - calcularIntensidad minutos}

calcularIntensidad :: Int -> Int
calcularIntensidad minutos = div minutos 4

{-
Paseo en barco: depende de cómo esté la marea
si está fuerte, aumenta el stress en 6 unidades y el cansancio en 10.
si está moderada, no pasa nada.
si está tranquila, el turista camina 10’ por la cubierta, aprecia la vista del “mar”, y sale a hablar con los tripulantes alemanes.
-}
paseoEnBarco :: String -> Turista -> Turista
paseoEnBarco marea unTurista
    |marea == "fuerte" = unTurista {nivelEstres = nivelEstres unTurista + 6, nivelCansancio= nivelCansancio unTurista + 10}
    |marea == "tranquila" = hablarIdioma "aleman".apreciarPaisaje "mar".caminar 10 $ unTurista
    |marea == "moderada" = unTurista

{-
Nos avisaron que es común que, cada cierto tiempo, se vayan actualizando las excursiones que ofrecen, en base a las nuevas demandas que surgen en el mercado turístico. 
-}


--Punto 1
{-
Crear un modelo para los turistas y crear los siguientes tres ejemplos:
Ana: está acompañada, sin cansancio, tiene 21 de stress y habla español.
Beto y Cathi, que hablan alemán, viajan solos, y Cathi además habla catalán. Ambos tienen 15 unidades de cansancio y stress.
-}
ana :: Turista
ana = UnTurista {
    nivelCansancio= 0,
    nivelEstres= 21,
    estaSolo= True,
    idiomas= ["español"]
}
beto :: Turista
beto = UnTurista {
    nivelCansancio= 15,
    nivelEstres= 15,
    estaSolo= False,
    idiomas= ["aleman"]
}
cathi :: Turista
cathi = UnTurista {
    nivelCansancio= 15,
    nivelEstres= 15,
    estaSolo= False,
    idiomas= ["aleman","catalan"]
}

{-
--Punto 2
Modelar las excursiones anteriores de forma tal que para agregar una excursión al sistema no haga falta modificar las funciones existentes. Además:
Hacer que un turista haga una excursión. Al hacer una excursión, el turista además de sufrir los efectos propios de la excursión, reduce en un 10% su stress.

Dada la función
deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

Definir la función deltaExcursionSegun que a partir de un índice, un turista y una excursión determine cuánto varió dicho índice después de que el turista haya hecho la excursión. Llamamos índice a cualquier función que devuelva un número a partir de un turista.
Por ejemplo, si “stress” es la función que me da el stress de un turista:
> deltaExcursionSegun stress ana irALaPlaya
-3     -- porque al ir a la playa Ana queda con 18 de estrés (21 menos 1 menos 10% de 20)
Usar la función anterior para resolver cada uno de estos puntos:
Saber si una excursión es educativa para un turista, que implica que termina aprendiendo algún idioma.
Conocer las excursiones desestresantes para un turista. Estas son aquellas que le reducen al menos 3 unidades de stress al turista.

-}