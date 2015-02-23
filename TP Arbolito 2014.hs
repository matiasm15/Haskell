type TipoArbolito = ([Char], Double, Double, Double)

misArboles =    [("jacaranda", 6.0, 1.0, 1.4),
                ("pino", 5.0, 3.0, 1.9),
                ("eucalipto", 5.0, 4.0, 0.7),   
                ("jacaranda", 10.0, 2.0, 1.0),  
                ("cerezo", 7.0, 11.0, 0.9), 
                ("ombu", 8.0, 10.0, 2.1)]

--a)
nombresFrondosos :: [TipoArbolito] -> [[Char]]
nombresFrondosos arboles = map darNombre (arbolesFrondosos arboles)

arbolesFrondosos arboles = filter tieneLaAlturaNecesaria arboles

tieneLaAlturaNecesaria (_, altura, ancho, _) =  altura >= 6 && 
                                                altura <= 15 &&
                                                ancho > altura

darNombre (nombre, _, _, _) = nombre

--b)
todosLosFrondososSonVitales :: [TipoArbolito] -> Bool
todosLosFrondososSonVitales arboles = all tieneBuenaVitalidad (arbolesFrondosos arboles)

tieneBuenaVitalidad (_, _, _, vitalidad) = vitalidad > 1

--c)
lluvia :: Double -> TipoArbolito -> TipoArbolito
lluvia milimetros (nombre, altura, ancho, vitalidad) = (nombre, altura + 1, ancho, vitalidad * (1 + (milimetros / 100)))

temperatura :: Double -> TipoArbolito -> TipoArbolito
temperatura grados (nombre, altura, ancho, vitalidad) | (grados < 0) = (nombre, altura, ancho, vitalidad * 0.5)
                                                      | (grados > 40) = (nombre, altura, ancho, vitalidad * 0.6)
                                                      | otherwise = (nombre, altura, ancho, vitalidad)

granizo :: TipoArbolito -> TipoArbolito
granizo (nombre, altura, ancho, vitalidad) = (nombre, altura / 2, ancho / 2, vitalidad)

--d)
factorClimaticoUniversal factor arboles = map factor arboles

-- nombresFrondosos [("jacaranda", 6.0, 1.0, 1.4), ("pino", 5.0, 3.0, 1.9), ("eucalipto", 5.0, 4.0, 0.7), ("jacaranda", 10.0, 2.0, 1.0), ("cerezo", 7.0, 11.0, 0.9), ("ombu", 8.0, 10.0, 2.1), ("helechoGigante", 7.0, 8.0, 0.7)]
-- ["cerezo","ombu","helechoGigante"]

-- todosLosFrondososSonVitales [("jacaranda", 6.0, 1.0, 1.4), ("pino", 5.0, 3.0, 1.9), ("eucalipto", 5.0, 4.0, 0.7), ("jacaranda", 10.0, 2.0, 1.0), ("cerezo", 7.0, 11.0, 0.9), ("ombu", 8.0, 10.0, 2.1)]
-- False

-- todosLosFrondososSonVitales [("jacaranda", 6.0, 1.0, 1.4), ("pino", 5.0, 3.0, 1.9), ("eucalipto", 5.0, 4.0, 0.7), ("jacaranda", 10.0, 2.0, 1.0), ("cerezo", 7.0, 11.0, 1.9), ("ombu", 8.0, 10.0, 2.1)]
-- True

-- lluvia 100 ("jacaranda", 6.0, 1.0, 1.4)
-- ("jacaranda",7.0,1.0,2.8)

-- temperatura 100 ("jacaranda", 6.0, 1.0, 1.4)
-- ("jacaranda",6.0,1.0,0.84)

-- temperatura 10 ("jacaranda", 6.0, 1.0, 1.4)
-- ("jacaranda",6.0,1.0,1.4)

-- temperatura (-10) ("jacaranda", 6.0, 1.0, 1.4)
-- ("jacaranda",6.0,1.0,0.7)

-- granizo ("jacaranda", 6.0, 1.0, 1.4)
-- ("jacaranda",3.0,0.5,1.4)

-- factorClimaticoUniversal (lluvia 50) [("jacaranda", 6.0, 1.0, 1.4), ("pino", 5.0, 3.0, 1.9), ("eucalipto", 5.0, 4.0, 0.7), ("jacaranda", 10.0, 2.0, 1.0), ("cerezo", 7.0, 11.0, 0.9), ("ombu", 8.0, 10.0, 2.1)]
-- [("jacaranda",7.0,1.0,2.1),("pino",6.0,3.0,2.85),("eucalipto",6.0,4.0,1.05),("jacaranda",11.0,2.0,1.5),("cerezo",8.0,11.0,1.35),("ombu",9.0,10.0,3.15)]


-- Parte 2
pino = ("pino", 3.2, 1.0)
ombu = ("ombu", 4.5, 4.2)
eucalipto = ("eucalipto", 9.0, 1.5)
jacaranda = ("jacaranda", 25.0, 0.7)
cerezo = ("cerezo", 2.5, 0.4)

caracteristicasNormales = [pino, ombu, eucalipto, jacaranda, cerezo]

laVerde =   [("jacaranda", 6.0, 1.0, 1.4),
            ("pino", 5.0, 3.0, 1.9),
            ("eucalipto", 5.0, 4.0, 0.7),
            ("jacaranda", 10.0, 2.0, 1.0),
            ("cerezo", 7.0, 11.0, 0.9),
            ("ombu", 8.0, 10.0, 2.1)]

campoLibre =    [("pino", 5.0, 1.0, 1.2),
                ("pino", 6.0, 0.8, 1.8),
                ("pino", 5.0, 1.1, 1.2),
                ("pino", 5.0, 1.5, 1.1),
                ("pino", 5.0, 0.9, 0.9),
                ("pino", 6.0, 1.1, 1.2),
                ("pino", 5.0, 1.6, 1.0)]

--a)
sePuedeTransplantar :: TipoArbolito -> [Char] -> Bool
sePuedeTransplantar (_, _, _, vitalidad) mes = (vitalidad > 1) && not (elem 'r' mes)

--b)
sePuedePodar :: TipoArbolito -> [Char] -> Bool
sePuedePodar (nombre, altura, ancho, vitalidad) mes = (sePuedeTransplantar (nombre, altura, ancho, vitalidad) mes) && (altura > darAlturaNormal nombre)

darAlturaNormal nombre = head(map darAltura (filter (coincideNombre nombre) caracteristicasNormales))

darAltura (_, altura, _) = altura

coincideNombre nombre (nombreArbol, _, _) = nombreArbol == nombre
