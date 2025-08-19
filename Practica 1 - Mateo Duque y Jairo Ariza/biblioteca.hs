import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Maybe (mapMaybe)


data Libro = Libro {
    codigo :: String,
    prestamo :: UTCTime,
    devolucion :: Maybe UTCTime
} deriving (Show, Read)

registrarPrestamo :: String -> UTCTime -> [Libro] -> [Libro]
registrarPrestamo codigoLibro tiempo biblioteca =
    Libro codigoLibro tiempo Nothing : biblioteca


registrarDevolucion :: String -> UTCTime -> [Libro] -> [Libro]
registrarDevolucion codigoLibro tiempo =
    map (\l -> if codigo l == codigoLibro then l { devolucion = Just tiempo } else l)


buscarLibro :: String -> [Libro] -> Maybe Libro
buscarLibro codigoLibro biblioteca =
    find (\l -> codigo l == codigoLibro) biblioteca


tiempoEnPrestamo :: Libro -> UTCTime -> NominalDiffTime
tiempoEnPrestamo libro tiempoActual =
    case devolucion libro of
        Just tiempoDev -> diffUTCTime tiempoDev (prestamo libro)
        Nothing        -> diffUTCTime tiempoActual (prestamo libro)

guardarBiblioteca :: [Libro] -> IO ()
guardarBiblioteca biblioteca = do
    resultado <- reintentar 5 (writeFile "Library.txt" (unlines (map mostrarLibro biblioteca)))
    case resultado of
        Left ex -> putStrLn $ "Error guardando la biblioteca: " ++ show ex
        Right _ -> putStrLn "Libro guardada en el archivo Library.txt."

reintentar :: Int -> IO a -> IO (Either IOException a)
reintentar 0 accion = catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
reintentar n accion = do
    resultado <- catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
    case resultado of
        Left _ -> do
            threadDelay 1000000 -- Esperar 1 segundo antes de reintentar
            reintentar (n - 1) accion
        Right val -> return (Right val)

cargarBiblioteca :: IO [Libro]
cargarBiblioteca = do
    resultado <- try (readFile "Library.txt") :: IO (Either IOException String)
    case resultado of
        Left ex -> do
            putStrLn $ "Error cargando la biblioteca: " ++ show ex
            return []
        Right contenido -> do
            let lineas = lines contenido
            return (map leerLibro lineas)
    where
        leerLibro linea = read linea :: Libro

mostrarLibro :: Libro -> String
mostrarLibro libro =
    show libro

leerBiblioteca :: IO [Libro]
leerBiblioteca = do
    contenido <- readFile "Library.txt"
    let lineas = lines contenido
    return (mapMaybe parsearLibro lineas)
  where
    parsearLibro :: String -> Maybe Libro
    parsearLibro linea = case words linea of
        [codigo, prestamo, devolucion] -> Just $ Libro codigo (read prestamo) (readMaybeDev devolucion)
        _ -> Nothing

    readMaybeDev :: String -> Maybe UTCTime
    readMaybeDev "Nothing"    = Nothing
    readMaybeDev devolucionStr = Just (read devolucionStr)

main :: IO ()
main = do
    biblioteca <- cargarBiblioteca
    putStrLn "¡Bienvenido al Sistema de Gestión de Biblioteca!"
    cicloPrincipal biblioteca

cicloPrincipal :: [Libro] -> IO ()
cicloPrincipal biblioteca = do
    putStrLn "Seleccione una opción:"
    putStrLn "1. Registrar préstamo de libro"
    putStrLn "2. Registrar devolución de libro"
    putStrLn "3. Buscar libro por código"
    putStrLn "4. Listar libros de la biblioteca"
    putStrLn "5. Salir"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el código del libro:"
            codigoLibro <- getLine
            tiempoActual <- getCurrentTime
            let bibliotecaAct = registrarPrestamo codigoLibro tiempoActual biblioteca
            guardarBiblioteca bibliotecaAct
            cicloPrincipal bibliotecaAct
        "2" -> do
            putStrLn "Ingrese el código del libro:"
            codigoLibro <- getLine
            tiempoActual <- getCurrentTime
            let bibliotecaAct = registrarDevolucion codigoLibro tiempoActual biblioteca
            guardarBiblioteca bibliotecaAct
            cicloPrincipal bibliotecaAct
        "3" -> do
            putStrLn "Ingrese el código del libro:"
            codigoLibro <- getLine
            tiempoActual <- getCurrentTime
            case buscarLibro codigoLibro biblioteca of
                Just libro -> do
                    let tiempoTotal = tiempoEnPrestamo libro tiempoActual
                    putStrLn $ "El libro con código " ++ codigoLibro ++ " esta préstamo."
                    putStrLn $ "Tiempo prestado: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "El libro no esta en prestamo / no esta en la biblioteca."
            cicloPrincipal biblioteca
        "4" -> do
            putStrLn "Mostrando lista de libros en la biblioteca"
            bibliotecaAct <- cargarBiblioteca
            mapM_ (\l -> putStrLn $ "Código: " ++ codigo l
                                  ++ ", Préstamo: " ++ show (prestamo l)
                                  ++ ", Devolución: " ++ show (devolucion l)) bibliotecaAct
            cicloPrincipal bibliotecaAct
        "5" -> putStrLn "¡Hasta pronto!"
        _   -> do
            putStrLn "Opción invalida. Por favor, seleccione una opción válida."
            cicloPrincipal biblioteca
