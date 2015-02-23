type Jugador = [Char]
type Participante = [Char]
type Club = [Char]
type Puesto = [Char]
type Cotizacion = Integer
type Puntaje = Integer
type MinutosJugados = Integer
type Evaluacion = (Jugador, Puntaje, MinutosJugados)
type Fecha = [Evaluacion]

participantes = [   ("Natalia", ["Abbondazieri","Lluy","Battaglia", "Lazzaro"]),
                    ("Romina", ["Islas", "Lluy", "Battaglia", "Lazzaro"]),
                    ("Jessica", ["Islas"])
                ]

clubes = ["Boca", "Racing", "Tigre"]

jugadores = [   ("Abbondazieri", "Boca", "Arquero", 6500000),
                ("Islas", "Tigre", "Arquero", 5500000),
                ("Lluy", "Racing", "Defensor", 1800000),
                ("Battaglia", "Boca", "Volante", 8000000),
                ("Lazzaro", "Tigre", "Delantero", 5200000)
            ]

darJugador (jugador, _, _, _) = jugador
darClub (_, club, _, _) = club
darPuesto (_, _, puesto, _) = puesto
darCotizacion (_, _, _, cotizacion) = cotizacion

fechas = [ quinta, sexta, septima, octava ]

quinta = [("Lluy", 8, 90), ("Lazzaro", 6, 90)]
sexta = [("Lazzaro", 7, 77), ("Islas", 6, 90), ("Lluy", 7, 90)]
septima = [("Battaglia", 13, 90), ("Lluy", 6, 90), ("Lazzaro", 8, 77)]
octava = [("Islas", 4, 84), ("Battaglia", 8, 90)]

darJugadorFecha (jugador, _, _) = jugador
darPuntaje (_, puntaje, _) = puntaje
darMinutosJugados (_, _, minutos) = minutos

-- 1) cotizacionDe, recibe el nombre de un jugador y devuelve su cotizacion.
cotizacionDe :: Jugador -> Cotizacion

cotizacionDe = darCotizacion . darFichaJugador
puestoDe = darPuesto . darFichaJugador

darFichaJugador jugador = head (filter (coincideJugador jugador) jugadores)

coincideJugador jugador = (== jugador) . darJugador

-- 2) cotizacionEquipoDe, recibe un participante y devuelve la cotizacion de su equipo.
cotizacionEquipoDe :: Participante -> Cotizacion
cotizacionEquipoDe participante = sum (map cotizacionDe (darJugadoresPorParticipante participante))

darJugadoresPorParticipante participante = (snd . head) (filter ((== participante) . fst) participantes)

-- 3) fueUsadoPor, recibe el nombre de un jugador y un participante y devuelve cierto o falso si el participante tiene a ese jugador en su equipo.
fueUsadoPor :: Jugador -> Participante -> Bool
fueUsadoPor jugador participante = any (== jugador) (darJugadoresPorParticipante participante)

-- 4) nombresJugadoresClub, recibe a un club y devuelve la lista de nombre de sus jugadores.
nombresJugadoresClub :: Club -> [Jugador]
nombresJugadoresClub club = map darJugador (filter ((== club) . darClub) jugadores)

-- 5) nombresTodoslosJugadores, devuelve la lista de los nombres de todos los jugadores participantes de torneo.
nombresTodoslosJugadores :: [Jugador]
nombresTodoslosJugadores = map darJugador todosLosJugadores

todosLosJugadores = filter estaEnUnEquipo jugadores

estaEnUnEquipo (_, club, _, _) = elem club clubes

-- 6) jugoFechaJugador, recibe una fecha y el nombre de un jugador y devuelve si jugo ese jugador en esa fecha.
jugoFechaJugador :: Fecha -> Jugador -> Bool
jugoFechaJugador fecha jugador = elem jugador (darJugadoresPorFecha fecha)

darJugadoresPorFecha fecha = map darJugadorFecha fecha

-- 7) minutosFechaJugador, recibe una fecha y el nombre de un jugador y devuelve los minutos jugados por ese jugador en esa fecha. Sino participo devuelve cero.
minutosFechaJugador :: Fecha -> Jugador -> Integer
minutosFechaJugador fecha jugador = aplicarFecha darMinutosJugados jugador fecha

aplicarFecha funcion jugador fecha | jugoFechaJugador fecha jugador = funcion (evaluacionFechaJugador jugador fecha)
                                   | otherwise = 0

evaluacionFechaJugador jugador fecha = head (filter (coincideJugadorFecha jugador) fecha)

coincideJugadorFecha jugador = (== jugador) . darJugadorFecha

-- 8) totalPuntosJugador, recibe el nombre de un jugador y devuelve el total de puntos logrados por ese jugador durante todo el torneo.
totalPuntosJugador :: Jugador -> Puntaje
totalPuntosJugador jugador = sum (map (puntosPorFecha jugador) fechas)

puntosPorFecha jugador fecha = aplicarFecha darPuntaje jugador fecha

-- 9) promedioPuntosJugador, recibe el nombre de un jugador y devuelve el promedio de puntos logrados por ese jugador durante todo el torneo.
promedioPuntosJugador :: Jugador -> Float
promedioPuntosJugador jugador = fromIntegral (totalPuntosJugador jugador) / fromIntegral (cantidadPartidosDisputados jugador)

cantidadPartidosDisputados jugador = length (filter (flip jugoFechaJugador jugador) fechas)

-- 10) mejorJugadorPor, recibe un criterio y devuelve el mejor jugador de acuerdo a ese criterio.
mejorJugadorPor criterio = fst(maximo (map (aplicarCriterio criterio) nombresTodoslosJugadores))

aplicarCriterio criterio jugador = (jugador, criterio jugador)

maximo [] = ("", 0)
maximo (x:xs) | snd x > snd (maximo xs) = x
              | otherwise = maximo xs

-- mejorJugadorPor totalPuntosJugador
-- "Lazzaro"

-- mejorJugadorPor cotizacionDe
-- "Battaglia"

-- mejorJugadorPor (flip puntosPorFecha quinta)
-- "Lluy"

-- mejorJugadorPor promedioPuntosJugador
-- "Battaglia"

-- 13) nombresJugadoresDeValorMenorADelPuesto, recibe un importe un puesto y devuelve los nombres de los jugadores de ese con una cotizacion menor o igual al importe recibido.
nombresJugadoresDeValorMenorADelPuesto :: Cotizacion -> Puesto -> [Jugador]
nombresJugadoresDeValorMenorADelPuesto cotizacion puesto = filter (cumpleCondicionParaPuesto cotizacion puesto) nombresTodoslosJugadores

cumpleCondicionParaPuesto cotizacion puesto jugador = (cotizacion >= (cotizacionDe jugador)) && puesto == (puestoDe jugador)
