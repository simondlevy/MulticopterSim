{--
  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

import Multicopter

motorfun :: Int -> Int
motorfun 0 = 0
motorfun _ = 1

main :: IO ()
main = runMulticopter motorfun
