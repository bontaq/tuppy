module Experiment.Liquid.Checker where

-- http://goto.ucsd.edu/~rjhala/liquid/liquid_types.pdf

import Experiment.Liquid.HM
import Parser

test = syntax $ clex 0 0 "main = 1"

-- since we already have HM inference, just stick with that
-- we can try out bidirectional checking later

valg = typeCheckCore test []
