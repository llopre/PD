module Main where

import qualified Data.ByteString.Lazy as BS
import System.IO
import System.Directory
import Data.List
import Data.Time.Clock
import Data.Time.Calendar
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad
import System.Exit (exitSuccess)

data  Persona = Persona { firstName :: String
                     , lastName :: String
                     } deriving (Show)


main :: IO()
main = do
    putStrLn "Seleccione una opción: "
    putStrLn "1. Cargar mis datos"
    putStrLn "2. Estaré enfermo de COVID?"
    putStrLn "3. Ya puedo salir?"
    putStrLn "4. Borrar mi hisopado y cargar uno nuevo"
    putStrLn "5. Salir"
    opcion <- getLine
    if (elem opcion ["1", "2", "3", "4", "5"]) then redireccion (opcion)
                                               else do
                                                putStrLn ("\nIngrese una opción correcta..\n")
                                                main

redireccion :: String -> IO()
redireccion "1" = do
                                putStrLn "Ingrese su nombre: "
                                nombre <- getLine
                                putStrLn "Ingrese su apellido"
                                apellido <- getLine
                                let user = Persona {firstName = nombre , lastName = apellido} 
                                putStrLn ("\nGracias " ++ mostrarDatoUser user "last" ++ "\n")   
                                
                                writeFile "usuario.txt" (mostrarDatoUser user "first" ++ " " ++mostrarDatoUser user "last")
                                main      
redireccion "2" = do
                                
                                putStrLn "Responda por si o por no:\n"
                                putStrLn "¿Ha tenido fiebre?"
                                res <- getLine
                                let p = calificarRespuesta (res == "si")
                                putStrLn $ show p
                                putStrLn "¿Ha tenido dolor de cabeza?"
                                res <- getLine
                                let counter = calificarRespuesta (res == "si")
                                p <- return (p + counter)
                                putStrLn $ show p
                                putStrLn "¿Ha tenido dolor de garganta?"
                                res <- getLine
                                let counter = calificarRespuesta (res == "si")
                                p <- return (p + counter)
                                putStrLn $ show p
                                if p > 1  then putStrLn "Sintomas compatibles con covid" 
                                          else putStrLn "Sin compatibilidad con covid"
redireccion "3"= do
                                let fileName = "hisopados.txt"
                                fileExist <- doesFileExist fileName

                                if not fileExist
                                then do 
                                    gestionArchivoHisopado fileName True -- Si es el primero, crea el archivo
                                    redireccion "3"

                                else do -- Primero buscar si esta cargada la fecha de ese usuario
                                    res <- hisopadoExistente getUsuario -- si da true ya esta cargada la fecha para el usuario
                                    if res
                                    then do
                                        --aca se analiza la fecha de fin de cuarentena
                                        hisopadosT <- readFile "hisopados.txt"
                                        let lineas = lines hisopadosT -- Todos los hisopados
                                        user <- getUsuario -- Usuario actual
                                        let res = head $ filter (isInfixOf user) lineas -- Busco la linea del usuario
                                        let nres = drop ((length res) - 10) res -- Me quedo con la fecha
                                        
                                        let dia = take 2 nres
                                        let mes = (drop 3 . take 5) nres
                                        let anio = (drop 6 . take 10) nres
                                        let fechaHisop = fromGregorian (read anio::Integer) (read mes::Int) (read dia::Int) 
                                        
                                        let (anioLibre, mesLibre, diaLibre) = toGregorian $ addDays (14) fechaHisop  
                                        
                                        bb <- diasRestantes anioLibre mesLibre diaLibre
                                    
                                        putStrLn ("Su cuarentena termina el " ++ show diaLibre ++ " de " 
                                              ++ toMonth mesLibre ++ " del " ++ show anioLibre)    
                                        
                                    else do
                                        gestionArchivoHisopado fileName False -- Agregar la fecha del usuario
                                        redireccion "3"

redireccion "4" = do 
                               buscaEnArchivo "hisopados.txt"
                    
redireccion "5" = exitSuccess



gestionArchivoHisopado :: String -> Bool -> IO()
gestionArchivoHisopado fn a = do
                             (dayH, monthH, yearh) <- solicitarHisopado
                             texto <- readFile "usuario.txt" 
                             if a then writeFile fn ( texto ++ " " ++ dayH ++ " " ++ monthH ++ " " ++ yearh)
                                  else appendFile fn (texto ++ " " ++ dayH ++ " " ++ monthH ++ " " ++ yearh)

mostrarDatoUser :: Persona -> String -> String
mostrarDatoUser (Persona {firstName = fn, lastName = ln}) a | a == "first" = fn
                                                            | a == "last"  = ln


solicitarHisopado :: IO(String, String, String)
solicitarHisopado = do
                      putStrLn "Fecha de su hisopado: "
                      putStrLn "Día(DD): "
                      dayH <- getLine
                      putStrLn "Mes(MM): "
                      monthH <- getLine
                      putStrLn "Año(YYYY):"
                      yearh <- getLine
                      return (dayH,monthH,yearh)

diasRestantes :: Integer -> Int -> Int -> IO()
diasRestantes a m d = do
                        ct <- getCurrentTime
                        let hoy = utctDay ct
                        let fechaLibre = fromGregorian a m d
                        let restantes = diffDays fechaLibre hoy
                        if (analizarDias restantes) then do putStrLn "Su cuarentena ha terminado!" 
                        else do
                            putStrLn ("Restan " ++ show restantes ++ " días")

analizarDias :: Integer -> Bool
analizarDias a | a > 0      = False
               | otherwise  = True

toMonth :: Int -> String
toMonth n
  | n>0 && n<13  = nombres !! (n-1)
  | otherwise    = error "Numero de mes incorrecto" 
 where nombres = words $ "Enero Febrero Marzo Abril Mayo Junio Julio"
                    ++ " Agosto Septiembre Octubre Noviembre Diciembre"


getUsuario :: IO String
getUsuario = readFile "usuario.txt"


hisopadoExistente :: IO String -> IO Bool
hisopadoExistente a = do
                        usuario <- a
                        --putStrLn usuario
                        hisopadosT <- readFile "hisopados.txt"
                        let res = check hisopadosT usuario
                        return res

check::[Char]->[Char]->Bool
check l s = check' l s True where
    check' _ [] h          = True
    check' [] _ h          = False
    check' (x:xs) (y:ys) h = (y == x && check' xs ys False) || (h && check' xs (y:ys) h)


calificarRespuesta :: Bool -> Integer
calificarRespuesta a | a == False = 0
                     | a == True  = 1


buscaEnArchivo :: String -> IO()
buscaEnArchivo file = do
                         res <- hisopadoExistente getUsuario
                         if res
                         then do
                                texto <- readFile file                                
                                let elTexto = lines texto -- lines lo hace lista de strings
                                --putStrLn "El texto con el usuario:"
                                --putStrLn $ unlines elTexto
                                user <- getUsuario 
                                let res = head $ filter (isInfixOf user) elTexto -- Busco la linea del usuario
                                putStrLn "la linea entera del usuario: "
                                putStrLn res
                                let nuevoTexto = delete res elTexto
                                let nuevoTexto2 = filter (not . null) nuevoTexto

                                removeFile file 
                                writeFile file (unlines nuevoTexto2) 
                                                                    
                                redireccion "3"

                          else do putStrLn "\nUsted no tiene un hisopado cargado\n"
                                  main

