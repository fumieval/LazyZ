module LazyZ.Interface where
import Debug.Trace
import Control.Applicative
import LazyZ.Encoding
import LazyZ.Expr
import System.IO
import qualified Data.Map as M
import Network

car x = apply x K
cdr x = apply x (K :$ I)

runLazyZWithSocket :: Expr e -> IO ()
runLazyZWithSocket = sequencial lazyZActions . fmap undefined . eval

getStdin :: IO (Expr Handle)
getStdin = return (Extern stdin)

getStderr :: IO (Expr Handle)
getStderr = return (Extern stderr)

getStdout :: IO (Expr Handle)
getStdout = return (Extern stdout)

sequencial :: (M.Map Int ([Expr e] -> IO (Expr e))) -> Expr e -> IO ()
sequencial _ (K :$ K) = return ()
sequencial action expr = sequencial' (car expr) (cdr expr)
    where
        sequencial' x f = ((action M.!) (decodeNum (car x)) $! toList (cdr x)) >>= sequencial action . apply f

getContentsFromExpr :: Expr Handle -> IO (Expr e)
getContentsFromExpr (Extern h) = fromString' <$> hGetContents h

getLineFromExpr :: Expr Handle -> IO (Expr e)
getLineFromExpr (Extern h) = fromString' <$> hGetLine h

putStrFromExpr :: Expr Handle -> Expr e -> IO (Expr e')
putStrFromExpr (Extern h) xs = hPutStr h (toString xs) >> return (K :$ K)

listenOnFromExpr :: Expr e -> IO (Expr Socket)
listenOnFromExpr port = Extern <$> listenOn (PortNumber $ toEnum $ decodeNum port)

acceptFromExpr :: Expr Socket -> IO (Expr Handle)
acceptFromExpr (Extern sock) = encode <$> accept sock
    where
        encode (h, host, port) = fromList [Extern h, fromString host, encodeNum (fromEnum port)]

sCloseFromExpr :: Expr Socket -> IO (Expr e)
sCloseFromExpr (Extern sock) = sClose sock >> return (K :$ K)

connectToFromExpr :: Expr e -> Expr e -> IO (Expr Handle)
connectToFromExpr host port = Extern <$> connectTo (toString host) (PortNumber $ toEnum $ decodeNum port)

-- この型はヤバイ
data HandleAndSocket = Handle Handle | Socket Socket
fromHandle (Handle x) = x -- この関数もヤバイ
fromSocket (Socket x) = x -- この関数も

lazyZActions :: M.Map Int ([Expr HandleAndSocket] -> IO (Expr HandleAndSocket))
lazyZActions = M.fromList $
    [ -- 0 ~ 9 : basic
	 (0, \(x:_) -> return x)
	,(1, \(x:_) -> return $! x) -- Strict
	-- 10 ~ 19: standard IO
	,(10, const $ fmap Handle <$> getStdin)
	,(11, const $ fmap Handle <$> getStdout)
	,(12, const $ fmap Handle <$> getStderr)
	,(13, \(x:_) -> fmap undefined <$> getContentsFromExpr (fromHandle <$> x))
	,(14, \(x:y:_) -> fmap undefined <$> putStrFromExpr (fromHandle <$> x) y)
	,(15, \(x:_) -> fmap undefined <$> getLineFromExpr (fromHandle <$> x))
    -- 20 ~ 29: socket
    ,(20, \(x:_) -> fmap Socket <$> listenOnFromExpr x)
    ,(21, \(x:_) -> fmap Handle <$> acceptFromExpr (fromSocket <$> x))
    ,(22, \(x:_) -> fmap undefined <$> sCloseFromExpr (fromSocket <$> x))
    ,(23, \(x:y:_) -> fmap Handle <$> connectToFromExpr x y)	
    ]