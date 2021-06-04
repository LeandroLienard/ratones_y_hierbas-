module Library where
import PdePreludat

data Raton = UnRaton{
   nombre ::String
,  edad :: Number
,  peso :: Number
,  enfermedades :: [Enfermedades]
} deriving (Show,Eq)

type Enfermedades = String

-- punto 1 

cerebro = UnRaton{
    nombre = "Cerebro"
    , edad = 9 
    ,peso = 0.2
    ,enfermedades = ["brucelosis","sarampion","tuberculosis"]
}

bicenterrata = UnRaton{
    nombre = "Bicenterrata"
    , edad = 256
    ,peso = 0.2
    ,enfermedades = []
} 

huesudo = UnRaton{
    nombre = "Huesudo"
    , edad = 4
    ,peso = 10
    ,enfermedades = ["alta obesidad","sinusitis"]
} 

--punto 2 TIPOS DE HIERBAS
type Hierba = Raton->Raton

--a
hierbaBuena :: Hierba 
hierbaBuena aRaton = aRaton {edad = sqrt(edad aRaton)}

--b
hierbaVerde :: String->Raton->Raton
hierbaVerde tipo = eliminarEnfermedadesCriterio(not.terminaEnTipo tipo) 

--eliminarEnfermedadesConTerminacion :: String->Raton->Raton
--eliminarEnfermedadesConTerminacion palabra aRaton = aRaton{enfermedades = filter (not.terminaEnTipo palabra) (enfermedades aRaton)} 

terminaEnTipo::String->String->Bool 
terminaEnTipo nombre nombre2 = drop (length nombre2 - length nombre)  nombre2 == nombre 

type Criterio = Enfermedades->Bool

eliminarEnfermedadesCriterio ::  Criterio-> Raton-> Raton
eliminarEnfermedadesCriterio criterio aRaton  = aRaton{enfermedades = filter criterio (enfermedades aRaton)} 

--c
alcachofa :: Hierba
alcachofa aRaton
    | peso aRaton > 2 = reducirPeso (0.1 * peso aRaton) aRaton
    | otherwise = reducirPeso (0.05 * peso aRaton) aRaton

reducirPeso :: Number->Raton->Raton
reducirPeso n aRaton = aRaton {peso = max 0 (peso aRaton - n) }

--d
hierbaZort:: Hierba
hierbaZort = transfomarseEnPinky

transfomarseEnPinky :: Raton->Raton
transfomarseEnPinky = quedaSano.recienNacido

quedaSano :: Raton->Raton
quedaSano aRaton = aRaton {enfermedades = []}

recienNacido:: Raton->Raton
recienNacido aRaton = aRaton {edad = 0}



--e
hierbaDelDiablo :: Hierba
hierbaDelDiablo = reducirPeso 0.1.recienNacido.eliminarEnfermedadesCortas

eliminarEnfermedadesCortas:: Raton->Raton
eliminarEnfermedadesCortas  = eliminarEnfermedadesCriterio ((>=10).length)


tomaHierba :: Raton->Hierba->Raton
tomaHierba aRaton hierba = hierba aRaton 

---Punto3
type Medicamento =[Hierba]

administrarMedicamentoA :: Raton->Medicamento->Raton
administrarMedicamentoA aRaton = foldl tomaHierba aRaton

--a
pondsAntiAge :: Medicamento 
pondsAntiAge = replicate 3 hierbaBuena ++ [alcachofa] 

--b
type Potencia = Number 

reduceFatFast :: Potencia->Medicamento
reduceFatFast potencia  =  [hierbaVerde "obesidad"] ++ replicate potencia alcachofa 

--c 
pdepCilina :: Medicamento
pdepCilina = map hierbaVerde sufijosInfecciosas

sufijosInfecciosas :: [String]
sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]


-- PUnto 4 : EXperimentos 
type Condicion = Number->Bool

--a
cantidadIdeal :: Condicion->Number
cantidadIdeal cond = head (filter cond numerosNaturales)

numerosNaturales :: [Number]
numerosNaturales  = iterate (1+) 1

--b
lograEstabilizar :: Medicamento->[Raton]->Bool
lograEstabilizar medicamento comunidad = all (ratonEstable) (ratonesMedicados medicamento comunidad)  

ratonesMedicados:: Medicamento->[Raton]->[Raton]
ratonesMedicados medicamento comunidad = map (flip (administrarMedicamentoA) medicamento)  comunidad 

--flip (administrarMedicamentoA ) pdepCilina huesudo 
ratonEstable :: Raton->Bool
ratonEstable aRaton =  (not.tieneSobrepeso) aRaton && (not.estaMuyEnfermo) aRaton

tieneSobrepeso :: Raton->Bool
tieneSobrepeso = (> 1).peso 

estaMuyEnfermo:: Raton->Bool
estaMuyEnfermo = (>=3).length.enfermedades 


--c

-- Diseñar el siguiente experimento: dado una comunidad de ratones, encontrar la potencia ideal del 
-- reduceFatFast necesaria para estabilizar la comunidad.
potenciaIdeal :: [Raton]->Potencia
potenciaIdeal  = primeroDe.potenciasValidas

primeroDe :: [Potencia]->Potencia
primeroDe [] = -1                   -- si devuelve -1 es porque no hay niguna potencia que lograEstabilizar
primeroDe lista = head lista

potenciasValidas :: [Raton]->[Potencia]
potenciasValidas comunidad = filter (cond comunidad .  reduceFatFast)  numerosNaturales

cond :: [Raton] -> Medicamento -> Bool
cond comunidad = flip (lograEstabilizar) comunidad  

--potenciaIdeal [huesudo ]
--[huesudo] con reduceFatFast 29 llega a lograEstabilizar

---5 Queremos saber si un medicamento logra estabilizar una comunidad infinita. ¿Podemos saberlo? Responder en estos dos casos:
{-
a) Si todos los ratones quedan con menos de 1kg y sin enfermedades. Justificar.

 Es imposible saberlo ya que la funcion "lograEstabilizar" funcion con un all ,que para ser Verdadero necesita evaluar todos los casos como
Verdadero , en este caso al ser infinitos nunca termina de evaluar todos los casos , por lo tanto nunca podria dar un resultado.


b)Si un ratón queda con 2kg y 4 enfermedades. Justificar.

En este caso, SI que se puede saber ya que la funcion usa un "all" ,esta funcional descubrir el caso Falso,ya devuelve Falso
independiemente de todos los demas casos. Entonces cuando encuentre al raton que queda con Sobrepeso y Muy enfermo , ya devolve Falso, es decir,
no logro estabilizar. 

EJEMPLO EN TESTS!!!
-}
 --2kg y 4 enfermedades. 

--6
{-
a) ¿Qué cambios debería hacer para agregar una nueva hierba y construir un medicamento con ella? ¿Habría que modificar las funciones existentes? 
No habria que realizar ningun cambio para agregar una nueva hierba, ya que cada funcion de hierbas es independiente de las demas porque se modelo
con funciones y no con data.

En el caso de querer hacer un nuevo medicamento tampoco habria que cambiar nada, solo habria que tener en cuenta que los medicamentos son 
lista de hierbas .


b)¿Qué concepto está involucrado en la pregunta anterior? ¿Para qué sirve en este caso?

Al concepto de hacer un codigo sostenible y modificable en el tiempo, ya que para sacar,modificar o agregar alguna hierba, esto no afectaria 
a todas las demas hierbas. 

c)Si se cambia el modelo del ratón, por ejemplo, ahora queremos que se registre el peso de un ratón siempre en libras. ¿Qué
 funciones habría que modificar?	

Solamente habria que cambiar la hierbaDelDiablo que le resta 0.1 kg al peso, habria que convertir 0.1 kg  a libras.
hierbaDelDiablo :: Hierba
hierbaDelDiablo = reducirPeso 0.1.recienNacido.eliminarEnfermedadesCortas

la funcion reducirPeso no necesitaria cambios, solo que sus entradas tambien esten en libras

reducirPeso :: Number->Raton->Raton
reducirPeso n aRaton = aRaton {peso = max 0 (peso aRaton - n) }

Tampoco habria que cambiar las funciones que usan el reducirPeso con porcentaje , ya que estas segurian usando el porcentaje del raton pero en libras

alcachofa :: Hierba
alcachofa aRaton
    | peso aRaton > 2 =  """reducirPeso(0.1 * peso aRaton)"""" aRaton
    | otherwise = reducirPeso (0.05 * peso aRaton) aRaton

Tampoco necesitan cambios los medicamentos, ya que estos usan hierbas ,y con solo cambiar los valores en hierbaDelDiablo ya estaria.
Mucho menos, los experimentos que usan medicamentos.
-}