{- |
Module      :  Rotas
Description :  Funções úteis para para criar e visualizar rotas

Maintainer  :  fc36188@alunos.fc.ul.pt
Stability   :  stable
Portability :  portable

Este módulo exporta um tipo de dados Rotas que representa um percurso, o seu respectivo nome e o nome de cada Ponto de paragem.
Exporta também duas funções convenientes para trabalhar com este tipo. 

-}

module Rotas
( Rota
, adicionaTecnica
, criaRota
) where

import Geometria
import Data.List (intercalate, splitAt)

data Rota = Rota {
    nome :: String, 
    paragens :: [Paragem]
}

data Paragem = Paragem {
    sitio :: String,
    ponto :: Ponto
}

instance Show Rota where
    show (Rota n pa) = n ++ " " ++ (show dist) ++ ": " ++ (intercalate " --- " (map sitio pa))
        where dist = round $ distanciaPercurso (map ponto pa)

instance Show Paragem where
    show (Paragem n p) = show n

criaRota :: String -> [String] -> Percurso -> Rota
criaRota name sitios percurso = Rota name (listaParagens sitios percurso)

adicionaTecnica :: Int -> Ponto -> Rota -> Rota
adicionaTecnica i ponto rota = Rota (nome rota) (adicionaAhLista i (criaParagemTecnica ponto) (paragens rota))

-- Funções auxiliares --
criaParagem :: String -> Ponto -> Paragem
criaParagem s p = Paragem s p

criaParagemTecnica :: Ponto -> Paragem
criaParagemTecnica p = criaParagem "(Pausa)" p

adicionaParagem :: Int -> Paragem -> Rota -> Rota
adicionaParagem i paragem rota = Rota (nome rota) (adicionaAhLista i paragem $ paragens rota)

removeParagemNum :: Int -> Rota -> Rota
removeParagemNum i rota = Rota (nome rota) (removeDaLista i (paragens rota))

listaParagens :: [String] -> Percurso -> [Paragem]
listaParagens sitios percurso = map (\x -> Paragem (fst x) (snd x)) (zip sitios percurso)
    
adicionaAhLista :: Int -> a -> [a] -> [a]
adicionaAhLista _ a [] = a:[]
adicionaAhLista 0 a xs = a:xs
adicionaAhLista i a (x:xs) = x:(adicionaAhLista (i - 1) a xs)

removeDaLista :: Int -> [a] -> [a]
removeDaLista _ [] = []
removeDaLista i (x:xs)
    | i == 0 = xs
    | otherwise = x : removeDaLista (i-1) xs