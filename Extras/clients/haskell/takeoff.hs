{--
  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

import Multicopter

motorfun :: [Double] -> [Double]
motorfun _ = [1,1,1,1]

main :: IO ()
main = runMulticopter motorfun
