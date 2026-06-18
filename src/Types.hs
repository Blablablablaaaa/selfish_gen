module Types where

import Codec.Picture (Image, Pixel8)
    
type Gene = Int
type Genotype = [Gene]

type Phenotype = Image Pixel8
data Direction = N | NE | E | SE | S | SW | W | NW  deriving (Enum)

