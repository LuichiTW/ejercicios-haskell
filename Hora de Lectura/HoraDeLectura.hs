
data Libro = UnLibro {
    titulo :: String,
    autor :: String,
    hojas :: Int
    } deriving (Show, Eq)

elVisitante :: Libro
elVisitante = UnLibro "El visitante" "Stephen King" 592

snk1 :: Libro
snk1 = UnLibro "Shingeki no Kyojin" "Hajime Isayama" 40

snk3 :: Libro
snk3 = UnLibro "Shingeki no Kyojin" "Hajime Isayama" 40

snk127 :: Libro
snk127 = UnLibro "Shingeki no Kyojin" "Hajime Isayama" 40

fundacion :: Libro
fundacion = UnLibro "Fundacion" "Isaac Asimov" 230

sandman5 :: Libro
sandman5 = UnLibro "Sandman" "Neil Gaiman" 35

sandman10 :: Libro
sandman10 = UnLibro "Sandman" "Neil Gaiman" 35

sandman12 :: Libro
sandman12 = UnLibro "Sandman" "Neil Gaiman" 35

eragon :: Libro
eragon = UnLibro "Eragon" "Christopher Paolini" 544

eldest :: Libro
eldest = UnLibro "Eldest" "Christopher Paolini" 704

brisingr :: Libro
brisingr = UnLibro "Brisingr" "Christopher Paolini" 763

legado :: Libro
legado = UnLibro "Legado" "Christopher Paolini" 811

biblioteca :: [Libro]
biblioteca = [elVisitante, snk1, snk3, snk127, fundacion, sandman5, sandman10, sandman12, eragon, eldest, brisingr, legado]

sagaDeEragon :: [Libro]
sagaDeEragon = [eragon, eldest, brisingr, legado]

esDe :: String -> Libro -> Bool
esDe autorDeLibro libro = autor libro == autorDeLibro

promedioDeHojas :: [Libro] -> Int
promedioDeHojas biblioteca = div (totalPaginasBiblioteca biblioteca) (cantidadDeLibros biblioteca)

totalPaginasBiblioteca :: [Libro] -> Int
totalPaginasBiblioteca biblioteca = sum (listaHojas biblioteca)

listaHojas :: [Libro] -> [Int]
listaHojas biblioteca = map hojas biblioteca

cantidadDeLibros :: [Libro] -> Int
cantidadDeLibros = length

esDeLecturaObligatoria :: Libro -> Bool
esDeLecturaObligatoria libro = esDe "Stephen King" libro || esSagaDeEragon libro || esFundacionIsaac libro

esSagaDeEragon :: Libro -> Bool
esSagaDeEragon libro = elem libro sagaDeEragon

esFundacionIsaac :: Libro -> Bool
esFundacionIsaac = esDe "Isaac Asimov"

esfantasiosa :: [Libro] -> Bool
esfantasiosa biblioteca = hayDeAutor "Christopher Paolini" biblioteca || hayDeAutor "Neil Gaiman" biblioteca

hayDeAutor :: String -> [Libro] -> Bool
hayDeAutor autor biblioteca = (elem autor.nombresBiblio) biblioteca

nombresBiblio :: [Libro] -> [String]
nombresBiblio biblioteca = map titulo biblioteca

nombreDeLaBiblioteca :: [Libro] -> String
nombreDeLaBiblioteca = sinVocales . concatenatoriaDeTitulos

sinVocales :: String -> String
sinVocales = filter (not . esVocal)

esVocal :: Char -> Bool
esVocal unCaracter = elem unCaracter "aeiouAEIOUÁÉÍÓÚ"

concatenatoriaDeTitulos :: [Libro] -> String
concatenatoriaDeTitulos unaBiblioteca = concatMap titulo unaBiblioteca

bibliotecaLigera :: [Libro] -> Bool
bibliotecaLigera biblioteca = any (>=40).listaHojas $ biblioteca 

