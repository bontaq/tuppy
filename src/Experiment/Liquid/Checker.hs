module Experiment.Liquid.Checker where

-- http://goto.ucsd.edu/~rjhala/liquid/liquid_types.pdf

import Experiment.Liquid.HM
import Parser

test = syntax $ clex 0 0 "main = 1"

valg = typeCheckCore test []
