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

data Paragem = Spot {
    nome :: String, 
    ponto :: Ponto
}

instance Show Rota where
    show (Rota n pa pu) = n ++ " " ++ ( show $ round $ distanciaPercurso pu) ++ ": " ++ (intercalate " --- " pa)
        where dist = 

instance Show Paragem where
    show (Spot n p) = show n

adicionaTecnica :: Int -> Ponto -> Rota -> Rota
adicionaTecnica i ponto rota = Rota (nome rota) (adicionaAhLista i (criaParagemTecnica ponto) (paragens rota))

criaParagem :: String -> Ponto -> Paragem
criaParagem s p = Paragem s p

criaParagemTecnica :: String -> Ponto -> Paragem
criaParagemTecnica p = Paragem "(Pausa)" p

adicionaParagem :: Int -> Paragem -> Rota -> Rota
adicionaParagem i paragem rota = Rota (nome rota) (adicionaAhLista i paragem $ paragens rota)

removeParagemNum :: Int -> Rota -> Rota
removeParagemNum i rota = Rota (nome rota) (removeDaLista i (paragens rota))

criaRota :: String -> [String] -> Percurso -> Rota
criaRota name sitios percurso = Rota name (listaParagens sitios percurso)

listaParagens :: [String] -> Percurso -> [Paragens]
listaParagens sitios percurso = let matchedList = zip sitios percurso
    in map Paragem matchedList
    
adicionaAhLista :: Int -> a -> [a] -> [a]
adicionaAhLista _ a [] = a:[]
adicionaAhLista 0 a xs = a:xs
adicionaAhLista i a (x:xs) = x:(adicionaAhLista (i - 1) a xs)

removeDaLista :: Int -> a -> [a] -> [a]
removeDaLista _ [] = []
removeDaLista i (x:xs)
    | i == 0 = xs
    | otherwise = a : removeDaLista (i-1) as

-- Maybe they want a type for the spots, another for the tecnical pauses... Work with k -> v maps, more show defenitions... Check book if have time
--  a versão master já entregue. se tiverres tempo acaba esta...
isEmpty :: Rota -> Bool
isEmpty r = paragens r == []

hasPause :: Rota -> Bool
hasPause r =  