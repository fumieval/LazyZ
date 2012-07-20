module LazyZ.Interface where
import Control.Applicative
import Control.Monad
import LazyZ.Encoding
import LazyZ.Expr
import System.IO
import qualified Data.Map as M
import Network

runLazyZ :: Expr e -> IO ()
runLazyZ expr = void $ lazyZeval $ fmap undefined expr :$ Extern (Function $ Extern . crystal . decodeNum)

lazyZeval :: LazyZExpr -> IO LazyZExpr
lazyZeval (f :$ g) = lazyZeval f >>= (lazyZeval g >>=) . lazyZapply
lazyZeval (Extern (IO m)) = m
lazyZeval x = return x

lazyZapply :: LazyZExpr -> LazyZExpr -> IO LazyZExpr
lazyZapply (Extern (Function f)) x = return $ f x
lazyZapply (Extern (IO m)) (Extern (IO n)) = m >>= (n >>=) . lazyZapply
lazyZapply x (Extern (IO m)) = m >>= lazyZapply x
lazyZapply (Extern (IO m)) x = m >>= flip lazyZapply x
lazyZapply I x = return x
lazyZapply (K :$ x) y = return x
lazyZapply (S :$ x :$ y) z = lazyZapply x z >>= (lazyZapply y z >>=) . lazyZapply
lazyZapply f g = return $ f :$ g

data LazyZExt = IO (IO LazyZExpr) | Function (LazyZExpr -> LazyZExpr) | Data LazyZData
type LazyZExpr = Expr LazyZExt

data LazyZData = Handle Handle | Socket Socket
fromHandle (Handle x) = x -- この関数もヤバイ
fromSocket (Socket x) = x -- この関数も

--data LazyZExpr = Pure (Expr HandleAndSocket) | Function (LazyZExpr -> LazyZExpr) | IO (IO LazyZExpr)

-- evalLazyZ :: LazyZExpr -> IO LazyZExpr

externdataIO :: IO LazyZData -> LazyZExpr
externdataIO x = Extern $ IO $ Extern <$> Data <$> x

car x = apply x K
cdr x = apply x (K :$ I)

hFlushFromExpr :: LazyZExpr -> LazyZExpr
hFlushFromExpr (Extern (Data (Handle h))) = Extern $ IO $ hFlush h >> return (K :$ K)

getContentsFromExpr :: LazyZExpr -> LazyZExpr
getContentsFromExpr (Extern (Data (Handle h))) = Extern $ IO $ fromString <$> hGetContents h

getLineFromExpr :: LazyZExpr -> LazyZExpr
getLineFromExpr (Extern (Data (Handle h))) = Extern $ IO $ fromString <$> hGetLine h

putStrFromExpr :: LazyZExpr -> LazyZExpr -> LazyZExpr
putStrFromExpr (Extern (Data (Handle h))) xs = Extern $ IO $ hPutStr h (toString xs) >> return (K :$ K)

listenOnFromExpr :: LazyZExpr -> LazyZExpr
listenOnFromExpr port = externdataIO $ Socket <$> listenOn (PortNumber $ toEnum $ decodeNum port)

acceptFromExpr :: LazyZExpr -> LazyZExpr
acceptFromExpr (Extern (Data (Socket sock))) = Extern $ IO $ encode <$> accept sock
    where
        encode (h, host, port) = fromList [Extern $ Data $ Handle h, fromString host, encodeNum (fromEnum port)]

sCloseFromExpr :: LazyZExpr -> LazyZExpr
sCloseFromExpr (Extern (Data (Socket sock))) = Extern $ IO $ sClose sock >> return (K :$ K)

connectToFromExpr :: LazyZExpr -> LazyZExpr -> LazyZExpr
connectToFromExpr host port = externdataIO $ Handle <$> connectTo (toString host) (PortNumber $ toEnum $ decodeNum port)

openFileFromExpr :: LazyZExpr -> LazyZExpr
openFileFromExpr path = externdataIO $ Handle <$> openFile (toString path) ReadWriteMode

crystal :: Int -> LazyZExt
crystal 0 = Function $ Extern . IO . return
crystal 1 = Function $ Extern . IO . (return $!)
crystal 2 = Function $ hFlushFromExpr
crystal 10 = Data $ Handle stdin
crystal 11 = Data $ Handle stdout
crystal 12 = Data $ Handle stderr
crystal 13 = Function  getContentsFromExpr
crystal 14 = Function $ Extern . Function . putStrFromExpr
crystal 15 = Function  getLineFromExpr
-- 20 ~ 29: socket
crystal 20 = Function listenOnFromExpr
crystal 21 = Function acceptFromExpr
crystal 22 = Function sCloseFromExpr
crystal 23 = Function $ Extern . Function . connectToFromExpr
-- 30 ~ 39: file
crystal 30 = Function openFileFromExpr