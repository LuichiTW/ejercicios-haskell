import Text.Show.Functions

data Participante = UnParticipante {
    nombre :: String,
    dinero :: Int,
    tactica :: String,
    propiedadesCompradas :: [Propiedad],
    acciones :: [Accion]
    } deriving Show

data Propiedad = UnaPropiedad {
    nombrePropiedad :: String,
    precio :: Int
    } deriving (Show, Eq)

type Accion = Participante -> Participante

carolina :: Participante
carolina = UnParticipante {
    nombre = "Carolina",
    dinero = 500,
    tactica = "Accionista",
    propiedadesCompradas = [],
    acciones = [pasarPorElBanco, pagarAAccionistas]
}

manuel :: Participante
manuel = UnParticipante {
    nombre = "Manuel",
    dinero = 500,
    tactica = "Oferente singular",
    propiedadesCompradas = [],
    acciones = [pasarPorElBanco, enojarse]
}

casita :: Propiedad
casita = UnaPropiedad {
    nombrePropiedad = "Casita",
    precio = 100
}

casota :: Propiedad
casota = UnaPropiedad {
    nombrePropiedad = "Casota",
    precio = 200
}

pasarPorElBanco :: Participante -> Participante
pasarPorElBanco participante = participante {dinero = dinero participante + 40, tactica = "Comprador compulsivo"}

enojarse :: Participante -> Participante
enojarse participante = participante {dinero = dinero participante + 50, acciones = gritar : acciones participante}

gritar :: Participante -> Participante
gritar participante = participante {nombre = "AHHHH" ++ nombre participante}

subastar :: Propiedad -> Participante -> Participante
subastar propiedad participante
    | tactica participante == "Accionista" = participante {dinero = dinero participante - precio propiedad, propiedadesCompradas = propiedad : propiedadesCompradas participante}
    | tactica participante == "Oferente singular" = participante {dinero = dinero participante - precio propiedad, propiedadesCompradas = propiedad : propiedadesCompradas participante}
    | otherwise = participante

cobrarAlquileres :: Participante -> Participante
cobrarAlquileres participante = participante {dinero = dinero participante + sum (dineroAAgregar.precioPropiedades$participante)}    

dineroAAgregar :: [Int] -> [Int]
dineroAAgregar precios = map propiedadBarata precios

propiedadBarata :: Int -> Int
propiedadBarata precio 
    |precio < 150 = 10
    |otherwise = 20

precioPropiedades :: Participante -> [Int]
precioPropiedades participante = map precio (propiedadesCompradas participante)

pagarAAccionistas :: Participante -> Participante
pagarAAccionistas participante
    | tactica participante == "Accionista" = participante {dinero = dinero participante + 200}
    | otherwise = participante {dinero = dinero participante - 100}