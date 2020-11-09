--Realizado por Carreño Hugo
--https://repl.it/@hugocarreno/tpo1funcional#main.hs
{- Instrumentos Musicales
[ Tuplas, Listas por comprensión y Orden Superior ]
1. Definir las siguientes funciones:
a. transformar que recibe una función de transformación de un valor a otro y una lista y devuelve una lista con el resultado de aplicar cada valor a la lista original.
Main> transformar doble [1..4]
[2,4,6,8] -}

doble :: Int -> Int
doble x = x * x

transformar :: (a -> b) -> [a] -> [b]
transformar funcion [] = []
transformar funcion (x:xs) = funcion(x):transformar funcion (xs)

{- b. filtrar que recibe una función criterio/condición y una lista y devuelve una lista con cada elemento que cumpla con la condición.
Main> filtrar even [1..10]
[2,4,6,8,10] -}

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar condicion [] = []
filtrar condicion (x:xs)| (condicion x) = x:(filtrar condicion xs)
                        | otherwise = filtrar condicion xs

{- c. find que recibe una función criterio/condición y una lista y devuelve el primer elemento (no una lista) que cumple con dicha condición.
Main> find even [1..10]
2  -}

find :: (a -> Bool) -> [a] -> a
find condicion [x] = x
find condicion (x:xs) = head (filtrar condicion xs) 

{- 2. Dado el siguiente dominio, donde cada instrumento es una tupla (modelo, tipo, costoBase, accesorios) y cada accesorio es una tupla donde (nombre, costoAccesorio)
instrumentos = [("gibson les paul", "guitarra", 2000, [("pickups", 10)]),("fender stratocaster", "guitarra", 1800, [("cuerdas", 5)]),("messiah", "violin", 3000, [("cuerdas", 5), ("puente", 15)]),("vendoma sjc 404", "violoncello", 3500, []),("hofner h42c", "violoncello", 4000, [("alma", 10), ("puente", 30)])]
a. Implementar las funciones extractoras para la tupla instrumento y accesorio. Sólo aquí podrá descomponer las tuplas. Por ejemplo:
Main> modelo ("gibson les paul", "guitarra", 2000, [("pickups", 10)])
"gibson les paul" -- ya que “gibson les paul” e -}

instrumentos = [("gibson les paul", "guitarra", 2000, [("pickups", 10)]),("fender stratocaster", "guitarra", 1800, [("cuerdas", 5)]),("messiah", "violin", 3000, [("cuerdas", 5),("puente", 15)]),("vendoma sjc 404", "violoncello", 3500, []),("hofner h42c", "violoncello", 4000, [("alma", 10), ("puente", 30)])]

modelo :: (a,b,c,d) -> a
modelo (a, b, c, d) = a
tipo :: (String, String, Int, [(String, Int)]) -> String
tipo (a, b, c, d) = b
costo :: (a,b,c,d) -> c
costo (a, b, c, d) = c
accesorios :: (String, String, Int, [(String, Int)]) -> [(String,Int)]
accesorios (a,b,c,d) = d
nombreAccesorio :: (String, Int) -> String
nombreAccesorio tupla = (fst tupla)
costoAccesorio :: (String, Int) -> Int
costoAccesorio tupla = (snd tupla)

{- b. Implementar las funciones siguientes utilizando las funciones del punto 1 y las funciones extractoras. No se pueden descomponer las tuplas en la siguientes definiciones.
i. esGuitarra que recibe una tupla instrumento y devuelve un booleano dependiendo el tipo de instrumento.
Main> esGuitarra ("gibson les paul","guitarra",2000, [("pickups",10)])
True -}

esGuitarra :: (String, String, Int, [(String,Int)]) -> Bool
esGuitarra tupla = (tipo tupla) == "guitarra"

{- ii. esViolin que recibe una tupla instrumento y devuelve un booleano dependiendo el tipo de instrumento.
Main> esViolin ("gibson les paul","guitarra",2000,[("pickups",10)])
False -}

esViolin :: (String, String, Int, [(String,Int)]) -> Bool
esViolin tupla = (tipo tupla) == "violin"

{- iii. costoTotal que recibe una tupla instrumento y devuelve el costo total (costo base + costo de los accesorios).
Main> costoTotal ("gibson les paul","guitarra",2000,[("pickups",10)])
2010 -}

costoTotal :: (String, String, Int, [(String,Int)]) -> Int
costoTotal tupla =  (costo tupla) + sum(transformar costoAccesorio (accesorios tupla))

{- iv. esCaro que recibe una tupla instrumento y devuelve True si el costo total supera 1500.
Main> esCaro ("gibson les paul","guitarra",2000,[("pickups",10)])
True
-}

esCaro :: (String, String, Int, [(String,Int)]) -> Bool
esCaro tupla = (costoTotal tupla) > 1500

{- c. Hacer las siguientes consultas en el intérprete utilizando las funciones definidas anteriormente, es decir, sin definir nuevas funciones ni listas por comprensión:
i. Costo total de todos los instrumentos que son violines-}

costoTotalViolines :: [(String, String, Int, [(String,Int)])] -> Int  
costoTotalViolines [] = 0
costoTotalViolines (x:xs) = sum (transformar costoTotal (filtrar (esViolin)(x:xs))) 

{- ii. Costo total de todos los instrumentos que son guitarras-}

costoTotalGuitarras :: [(String, String, Int, [(String,Int)])] -> Int  
costoTotalGuitarras [] = 0
costoTotalGuitarras (x:xs) = sum (transformar costoTotal (filtrar (esGuitarra)(x:xs)))

{-iii. Costo total de todos los instrumentos caros (no importa de qué tipo)-}

costoTotalCaros :: [(String, String, Int, [(String,Int)])] -> Int  
costoTotalCaros [] = 0
costoTotalCaros (x:xs) = sum (transformar costoTotal (filtrar (esCaro)(x:xs)))

{-iv. La primera guitarra cara
Incluir estas consultas en el archivo de texto como comentarios. -}

guitarraCara :: [(String, String, Int, [(String,Int)])] -> (String, String, Int, [(String,Int)])
guitarraCara [x] = x
guitarraCara (x:xs) =  find (esGuitarra)(x:(filtrar (esCaro) (x:xs)))
--guitarraCara (x:xs)| (esGuitarra x) && (esCaro x) = [x]++(guitarraCara xs)
  --                  | otherwise = []++(guitarraCara xs)

{- 3. Responder:
a. ¿Qué necesitaría implementar para hacer las mismas consultas con respecto a los violoncellos? -}

--Deberia implementar una funcion esVioloncello, y costoTotalVioloncello de funcionamiento similar a las anteriores ya mencionadas como esGuitarra o costoTotalGuitarras

{-b. ¿Qué ventaja ve en que las funciones definidas en el punto 1 reciban una función por parámetro?
Incluir estas respuestas en el archivo como comentarios.-}

--la ventaja es que retornan funciones de resultado, creando asi funciones de orden superior, ademas se ahorran lineas de codigo, es mas escalable el programa.

main = do
  print("transformar doble [3,5,-4]")
  print(transformar doble [3,5,-4])
  print("filtrar even [1,2,3,4,5,6,7,8,9,10]")
  print(filtrar even [1,2,3,4,5,6,7,8,9,10])
  print("find even [1,2,3,4,5,6,7,8,9,10]")
  print(find even [1,2,3,4,5,6,7,8,9,10])
  print("modelo ('gibson les paul', 'guitarra', 2000, [('pickups', 10)])")
  print(modelo ("gibson les paul", "guitarra", 2000, [("pickups", 10)]))
  print("tipo ('gibson les paul', 'guitarra', 2000, [('pickups', 10)])")
  print(tipo ("gibson les paul", "guitarra", 2000, [("pickups", 10)]))
  print("costo ('gibson les paul', 'guitarra', 2000, [('pickups', 10)])")
  print(costo ("gibson les paul", "guitarra", 2000, [("pickups", 10)]))
  print("accesorios ('gibson les paul', 'guitarra', 2000, [('pickups', 10),('funda',200)])")
  print(accesorios ("gibson les paul", "guitarra", 2000, [("pickups", 10),("funda",200)]))
  print("nombreAccesorio ('pickups',10)")
  print(nombreAccesorio ("pickups",10))
  print("costoAccesorio ('pickups',10)")
  print(costoAccesorio ("pickups",10))
  print("esGuitarra ('gibson les paul','guitarra',2000, [('pickups',10)])")
  print(esGuitarra ("gibson les paul","guitarra",2000, [("pickups",10)]))
  print("esViolin ('gibson les paul','guitarra',2000, [('pickups',10)])")
  print(esViolin ("gibson les paul","guitarra",2000, [("pickups",10)]))
  print("costoTotal ('gibson les paul','guitarra',2000, [('pickups',10)])")
  print(costoTotal ("gibson les paul","guitarra",2000, [("pickups",10)]))
  print("esCaro ('gibson les paul','guitarra',2000,[('pickups',10)])")
  print(esCaro ("gibson les paul","guitarra",2000,[("pickups",10)]))
  print("costoTotalViolines instrumentos")
  print(costoTotalViolines instrumentos)
  print("costoTotalGuitarras instrumentos")
  print(costoTotalGuitarras instrumentos)
  print("costoTotalCaros instrumentos")
  print(costoTotalCaros instrumentos)
  print("guitarraCara instrumentos")
  print(guitarraCara instrumentos)