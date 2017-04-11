{-# LANGUAGE RankNTypes #-}

module KindLang.Parser.State where

import KindLang.Data.AST(newNode,ASTNodeInfo)
import KindLang.Data.KStat
import Text.Parsec
import Control.Monad.Trans

type ParseState = ()
type ParseMonad s = KStat s

type ParserS s r = ParsecT String ParseState (ParseMonad s) r
type Parser r = forall s . ParsecT String ParseState (ParseMonad s) r

newNodeP :: (ASTNodeInfo -> r) -> Parser r
newNodeP constructor = lift $ newNode constructor
                       
