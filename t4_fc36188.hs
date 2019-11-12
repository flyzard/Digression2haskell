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
import Data.List (intercalate)

data Rota = Rota {
    nome :: String, 
    paragens :: [String],
    percurso :: Percurso
}

instance Show Rota where
    show (Rota n pa pu) = n ++ " " ++ ( show $ round $ distanciaPercurso pu) ++ ": " ++ (intercalate " --- " pa)

adicionaTecnica :: Int -> Ponto -> Rota -> Rota
adicionaTecnica i ponto rota = Rota (nome rota) (adicionaAhLista i "(Pausa)" (paragens rota)) (adicionaAhLista i ponto (percurso rota))

criaRota :: String -> [String] -> Percurso -> Rota
criaRota name places percurso = let matchedList = zip places percurso
    in Rota name (map fst matchedList) (map snd matchedList)
    
adicionaAhLista :: Int -> a -> [a] -> [a]
adicionaAhLista _ a [] = a:[]
adicionaAhLista 0 a xs = a:xs
adicionaAhLista i a (x:xs) = x:(adicionaAhLista (i - 1) a xs)
