type TipoVehiculo = ([Char], [([Char], Double, Double)])
type TipoPieza = ([Char], Double)
type TipoRequerimiento = ([Char], [TipoPieza])

vehiculos = [("bicicleta",[("manubrio",1.0,50.0),("asiento",1.0,70.0),("rueda",2.0,20.0),("corneta",1.0,10.0)]),("bicicleta",[("asiento",1.0,35.0),("rueda",1.0,20.0)]),("uniciclo",[("rueda",1.0,30.0),("asiento",1.0,50.0),("calcomanias",4.0,1.0)]),("triciclo",[("manubrio",1.0,40.0),("asiento",1.0,70.0),("rueda",2.0,20.0)]),("patineta",[("tabla",1.0,80.0),("rueda",3.0,5.0)])]

requerimientos = [("bicicleta",[("manubrio",1.0),("asiento",1.0),("rueda",2.0)]),("uniciclo",[("rueda",1.0),("asiento",1.0)]),("triciclo",[("rueda",3.0),("asiento",1.0)]),("patineta",[("tabla",1.0),("rueda",4.0)])]

--1)
valor :: TipoVehiculo -> Double
valor (_, listaPiezas) = sum(map valorPiezas listaPiezas)

valorPiezas(_, cantidad, precio) = cantidad * precio

--2) a)
tiene :: TipoVehiculo -> TipoPieza -> Bool
tiene (_, listaPiezas) requerimiento = any (esSuficiente requerimiento) listaPiezas

esSuficiente (piezaReq, cantidadReq) (piezaPos, cantidadPos, _) = (piezaReq == piezaPos) && (cantidadPos >= cantidadReq)

--b)
tieneTodas :: TipoVehiculo -> [TipoPieza] -> Bool
tieneTodas vehiculo listaPiezas = all (tiene vehiculo) listaPiezas

--c)
rodadosCompletos :: [TipoVehiculo] -> [TipoRequerimiento] -> [TipoVehiculo]
rodadosCompletos listaVehiculos listaRequerimientos = filter (verificarPiezas listaRequerimientos) listaVehiculos

verificarPiezas listaRequerimientos vehiculo = any (cumpleReq vehiculo) listaRequerimientos

cumpleReq vehiculo (nombreVehiculo, listaPiezas) = (fst vehiculo == nombreVehiculo) && (tieneTodas vehiculo listaPiezas)

--3)
totalALaVenta :: [TipoVehiculo] -> [TipoRequerimiento] -> Double
totalALaVenta listaVehiculos listaRequerimientos = sum(map valor (rodadosCompletos listaVehiculos listaRequerimientos)) * 1.21

--4) a)
tiposDeVehiculos :: [TipoVehiculo] -> [[Char]]
tiposDeVehiculos listaVehiculos = eliminarRepetidos(map darNombreVehiculo listaVehiculos)

darNombreVehiculo (nombreVehiculo, _) = nombreVehiculo

eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | elem x xs = eliminarRepetidos xs
                         | otherwise = x:(eliminarRepetidos xs)

--b)
cantidadPorTipoQueCumplan :: (TipoVehiculo -> Bool) -> [TipoVehiculo] -> [([Char], Int)]
cantidadPorTipoQueCumplan f listaVehiculos = map (cantidadQueCumplan f listaVehiculos) (tiposDeVehiculos listaVehiculos)

cantidadQueCumplan f listaVehiculos nombreIrrepetible = (nombreIrrepetible, length (filter f (buscarElemento listaVehiculos nombreIrrepetible)))

buscarElemento listaDominio nombreParametro = [elemento | elemento <- listaDominio, (fst elemento) == nombreParametro]

--c) i)
esDeLujo :: TipoVehiculo -> Bool
esDeLujo vehiculo = (totalALaVenta [vehiculo] requerimientos) > 300

--ii)
tieneAlgoQueSaleMasDe :: Double -> TipoVehiculo -> Bool
tieneAlgoQueSaleMasDe numero (_, listaPiezas) = any (numero < ) (map darPrecio listaPiezas)

darPrecio (piezaPoseida, cantidad, precio) = precio

--iii)
esComplejo :: TipoVehiculo -> Bool
esComplejo (_, listaPiezas) = (length listaPiezas) > 4
