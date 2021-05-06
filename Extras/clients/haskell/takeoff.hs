{--
  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

import Multicopter

motorfun :: [Double] -> [Double]
motorfun [0] = [1,1,1,1]
motorfun _ = [0,0,0,0]

main :: IO ()
main = runMulticopter motorfun
