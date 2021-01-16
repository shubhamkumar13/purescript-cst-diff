{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Text.IO as T
import qualified Data.Text as Text
import Data.Text.Encoding as T 
import Data.ByteString.Lazy.UTF8 as BS
import qualified Language.PureScript.CST as CST
import System.IO as IO
import Text.JSON
import Text.JSON.Generic ( toJSON )
import Data.Bifunctor ( Bifunctor(second) )
import Text.JSON.Pretty ( pp_value )
import Text.PrettyPrint

main :: IO ()
main = do
    src <- getTokens filePath
    case src of
        Left a -> Prelude.putStrLn $ show a
        Right b ->  
            -- Prelude.putStrLn (show $ toJSONList b) >>
            IO.writeFile "something.txt" (render $ hcat (map (pp_value . f) b)) >> 
            Prelude.putStrLn ""
            -- Prelude.putStrLn (show . head $ map f b)

filePath = "Main.purs"

getTokens filePath = do
    f <- T.readFile filePath
    case sequence $ CST.lex f of
        Left (_, err) -> pure $ Left err
        Right toks -> pure $ Right toks

f CST.SourceToken{..} = 
    -- (show tokAnn, show tokValue)
    JSObject $ toJSObject [("SourceToken", JSArray [g1 tokAnn, g2 tokValue])]
g1 CST.TokenAnn{..} = 
    -- (show tokRange, show tokLeadingComments, show tokTrailingComments)
    JSObject $ toJSObject [("TokenAnn", h tokRange)]


g2 x = JSObject $ toJSObject [Data.Bifunctor.second toJSON a] 
    where
        a =
            case x of
                CST.TokLeftParen -> ("TokLeftParen", [show x])
                CST.TokRightParen -> ("TokRightParen", [show x])
                CST.TokLeftBrace -> ("TokLeftBrace", [show x])
                CST.TokRightBrace -> ("TokRightBrace", [show x])
                CST.TokLeftSquare -> ("TokLeftSquare", [show x])
                CST.TokRightSquare -> ("TokRightSquare", [show x])
                CST.TokLeftArrow a -> ("TokLeftArrow", [show a])
                CST.TokRightArrow a -> ("TokRightArrow", [show a])
                CST.TokRightFatArrow a -> ("TokRightFatArrow", [show a])
                CST.TokDoubleColon a -> ("TokDoubleColon", [show a])
                CST.TokForall a -> ("TokForall", [show a])
                CST.TokEquals -> ("TokEquals", [show x])
                CST.TokPipe -> ("TokPipe", [show x])
                CST.TokTick -> ("TokTick", [show x])
                CST.TokDot -> ("TokDot", [show x])
                CST.TokComma -> ("TokComma", [show x])
                CST.TokUnderscore -> ("TokUnderscore", [show x])
                CST.TokBackslash -> ("TokBackslash", [show x])
                CST.TokLowerName ys y -> ("TokLowerName", [show ys, show y])
                CST.TokUpperName ys y -> ("TokUpperName", [show ys, show y])
                CST.TokOperator ys y -> ("TokOperator", [show ys, show y])
                CST.TokSymbolName ys y -> ("TokSymbolName", [show ys, show y])
                CST.TokSymbolArr a -> ("TokSymbolArr", [show a])
                CST.TokHole y -> ("TokHole", [show y])
                CST.TokChar y ch -> ("TokChar", [show y, show ch])
                CST.TokString y z -> ("TokString", [show y, show z])
                CST.TokRawString y -> ("TokRawString", [show y])
                CST.TokInt y i -> ("TokInt", [show y, show i])
                CST.TokNumber y d -> ("TokNumber", [show y, show d])
                CST.TokLayoutStart -> ("TokLayoutStart", [show x])
                CST.TokLayoutSep -> ("TokLayoutSep", [show x])
                CST.TokLayoutEnd -> ("TokLayoutEnd", [show x])
                CST.TokEof -> ("TokEof", [show x])

h CST.SourceRange{..} = JSObject $ toJSObject [("SourceRange", JSObject $ toJSObject [("srcStart", k srcStart), ("srcEnd", k srcEnd)])]

k CST.SourcePos{..} =  
    JSObject $
    toJSObject 
        [("SourcePos"
        , JSObject $ 
            toJSObject 
            [("srcLine", toJSON srcLine)
           , ("srcColumn", toJSON srcColumn)])]