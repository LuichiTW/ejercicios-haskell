import Text.Show.Functions()

data Autor = UnAutor {
    nombre :: String,
    obras :: [Obra]
} deriving (Show, Eq)

data Obra = UnaObra {
    titulo :: String,
    anio :: Int
} deriving (Show, Eq)

obra1 :: Obra
obra1 = UnaObra "Había una vez un pato." 1997

obra2 :: Obra
obra2 = UnaObra "¡Habia una vez un pato!" 1996

obra3 :: Obra
obra3 = UnaObra "Mirtha, Susana y Moria." 2010

obra4 :: Obra
obra4 = UnaObra "La semántica funcional del amoblamiento vertebral es riboficiente" 2020

obra5 :: Obra
obra5 = UnaObra "La semántica funcional de Mirtha, Susana y Moria." 2022

autor1 :: Autor
autor1 = UnAutor "Mirtha Legrand" [obra1]

autor2 :: Autor
autor2 = UnAutor "Susana Gimenez" [obra2, obra3]

autor3 :: Autor
autor3 = UnAutor "Moria Casan" [obra4, obra5]

textoCrudo :: String -> String
textoCrudo texto = filter esLetraONum.sacarTildes $ texto

esLetraONum :: Char -> Bool
esLetraONum letra = elem letra ['a'..'z'] || elem letra ['A'..'Z'] || elem letra ['1'..'9']

sacarTildes :: String -> String
sacarTildes texto = map reemplazarVocalesConTilde texto

reemplazarVocalesConTilde :: Char -> Char
reemplazarVocalesConTilde letra
  | letra == 'á' = 'a'
  | letra == 'é' = 'e'
  | letra == 'í' = 'i'
  | letra == 'ó' = 'o'
  | letra == 'ú' = 'u'
  | otherwise = letra

esPosterior :: Obra -> Obra -> Bool
esPosterior obra1 obra2 = anio obra1 > anio obra2

copiaLiteral :: Obra -> Obra -> Bool
copiaLiteral obraPlagio obraPlagiada = textoCrudo (titulo obraPlagio) == textoCrudo (titulo obraPlagiada) && esPosterior obraPlagio obraPlagio

comienzaIgual :: Int -> Obra -> Obra -> Bool
comienzaIgual cantidad obraPlagio obraPlagiada = take cantidad (textoCrudo (titulo obraPlagio)) == take cantidad (textoCrudo (titulo obraPlagiada)) && esPosterior obraPlagio obraPlagiada

leAgregaronIntro :: Obra -> Obra -> Bool
leAgregaronIntro obraPlagio obraPlagiada
  |null (titulo obraPlagio) = False
  |otherwise = titulo obraPlagio == titulo obraPlagiada || leAgregaronIntro (quitarLetras obraPlagio) obraPlagiada

quitarLetras :: Obra -> Obra
quitarLetras obra = obra {titulo = drop 1 (titulo obra)}

esCopia :: Obra -> Obra -> Bool
esCopia = \obraPlagio obraPlagiada -> copiaLiteral obraPlagio obraPlagiada || comienzaIgual 5 obraPlagio obraPlagiada || leAgregaronIntro obraPlagio obraPlagiada

data Bot = UnBot {
    fabricante :: String,
    formasPlagio :: [Plagio]
} deriving (Show)

type Plagio = Obra -> Obra -> Bool

bot1 :: Bot
bot1 = UnBot "PlagioYa" [copiaLiteral, comienzaIgual 5, leAgregaronIntro]

bot2 :: Bot
bot2 = UnBot "PlagioYa" [copiaLiteral, comienzaIgual 5, leAgregaronIntro]

detectarPlagio :: Bot -> Plagio
detectarPlagio bot obraPlagio obraPlagiada = any (\plagio -> plagio obraPlagio obraPlagiada) (formasPlagio bot) 

esCadenaDePlagios :: Bot -> [Autor] -> Bool
esCadenaDePlagios bot [_] = True
esCadenaDePlagios bot autor = autoresPlagianEntreSi bot (head autor) (head (tail autor)) && esCadenaDePlagios bot (tail autor)
  
autoresPlagianEntreSi :: Bot -> Autor -> Autor -> Bool
autoresPlagianEntreSi bot autor1 autor2
  |null (obras autor1) = False
  |otherwise = buscarPlagio bot autor1 autor2 || autoresPlagianEntreSi bot autor1 {obras = tail (obras autor1)} autor2

buscarPlagio :: Bot -> Autor -> Autor -> Bool
buscarPlagio bot autor1 autor2 = any (detectarPlagio bot (head.obras $ autor1)) (obras autor2)

plagiadorArrepentido :: Bot -> Autor -> Autor -> Bool
plagiadorArrepentido bot autor1 autor2 = (1==).cantidadDePlagios bot autor1 $ autor2

cantidadDePlagios :: Bot -> Autor -> Autor -> Int
cantidadDePlagios bot autor1 autor2
  |null (obras autor1) = 0
  |otherwise = contarPlagios bot autor1 autor2 + cantidadDePlagios bot autor1 {obras = tail (obras autor1)} autor2

contarPlagios :: Bot -> Autor -> Autor -> Int
contarPlagios bot autor1 autor2 = length (filter (detectarPlagio bot (head.obras $ autor1)) (obras autor2))