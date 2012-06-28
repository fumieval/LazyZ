
module LazyZ.Evaluator where

import Control.Applicative
import LazyZ.Encoding
import LazyZ.Expr
import System.IO
import qualified Data.Map as M
import Network
{-
stdin :: IO Expr e
stdin = fromString . getContents

stdout :: Expr e -> IO ()
stdout = putStr . map chr . takeWhile (<256) . decodeNum . split
-}

-- アクションの番号と引数のリスト、アクションの結果を引数に取り次の対を返す関数の対。
sequencial :: (M.Map Int ([Expr e] -> IO (Expr e))) -> Expr e -> IO ()
sequencial _ (K :$ K) = return ()
sequencial action expr = sequencial' (apply expr K) (apply expr (K :$ I))
    where
        sequencial' x f = (action M.! decodeNum (apply x K)) (toList $ apply x (K :$ I)) >>= sequencial action . apply f

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

data HandleAndSocket = Handle Handle | Socket Socket -- | T2 c
fromHandle (Handle x) = x
fromSocket (Socket x) = x

socketActions :: M.Map Int ([Expr HandleAndSocket] -> IO (Expr HandleAndSocket))
socketActions = M.fromList $
    [(10, \(x:_) -> fmap undefined <$> sCloseFromExpr (fromSocket <$> x))
    ,(11, \(x:_) -> fmap Socket <$> listenOnFromExpr (fromHandle <$> x))
    ,(12, \(x:_) -> fmap Handle <$> acceptFromExpr (fromSocket <$> x))
    ,(13, \(x:y:_) -> fmap Handle <$> connectToFromExpr x y)
    ]