



data Equipo = Equipo {
nombreEquipo :: String,
grupo        :: Grupo,
listaDeJugadores :: [Jugador]
}
type Grupo = Char

data Jugador = Jugador {
nombre :: String,
edad :: Float,
promedioDeGol :: Float,
habilidad :: Float,
valorDeCansancio :: Float
}

esJoven :: Jugador -> Bool 
esJoven = (<27).edad 

figurasDelEquipo :: Equipo -> [Jugador]
figurasDelEquipo unEquipo = filter sonCracks (listaDeJugadores unEquipo)

sonCracks :: Jugador -> Bool 
sonCracks jugador = habilidoso jugador && goleador jugador

habilidoso :: Jugador -> Bool 
habilidoso = (>75) . habilidad

goleador :: Jugador -> Bool 
goleador = (>0).promedioDeGol

-- PUNTO 2 --
tieneFarandulero :: Equipo -> Bool 
tieneFarandulero jugadores = any esFarandulero (listaDeJugadores jugadores) 

esFarandulero :: Jugador -> Bool 
esFarandulero jugador = nombre jugador `elem` jugadoresFaranduleros 

jugadoresFaranduleros :: [String]
jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

--PUNTO 3 --

figuritasDificles :: [Equipo] -> Grupo -> [[Jugador]]
figuritasDificles equipos unGrupo 
    | all ((== unGrupo).grupo) equipos = map jugadoresDificiles equipos   

jugadoresDificiles :: Equipo -> [Jugador]
jugadoresDificiles unEquipo = filter esDificil (listaDeJugadores unEquipo)

esDificil :: Jugador -> Bool 
esDificil unJugador = sonCracks unJugador && (not.esFarandulero) unJugador && esJoven unJugador

-- PUNTO 4 --

jugarPartido :: Equipo -> Equipo 
jugarPartido unEquipo = unEquipo { listaDeJugadores = modificarJugadores (listaDeJugadores unEquipo)}

modificarJugadores :: [Jugador] -> [Jugador]
modificarJugadores = map cansancioIndividual 

cansancioIndividual :: Jugador -> Jugador
cansancioIndividual unJugador 
    | esDificil unJugador= aumentarCansancio (const 50) unJugador
    | esJoven unJugador = aumentarCansancio (+ (10 * valorDeCansancio unJugador / 100)) unJugador
    | sonCracks unJugador = aumentarCansancio (+ 20) unJugador
    | otherwise = aumentarCansancio (+ valorDeCansancio unJugador * 2) unJugador


aumentarCansancio :: (Float -> Float) -> Jugador -> Jugador 
aumentarCansancio f unJugador = unJugador {valorDeCansancio = f $ valorDeCansancio unJugador}



