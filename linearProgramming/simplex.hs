-- Maximize c^tranpose ip x
-- subject to
--   matvecMult A x = b
--

example: minimize -2x - 3y - 4z
s.t.
  3x + 2y + z <= 10
  2x + 5y + 3z <= 15
  x,y,z >= 0

-- pushing this to a Tableau in canonical form
--
--

import Data.List

transpose [[1,2,3,4,0,0,0],[0,3,2,1,1,0,10],[0,2,5,3,0,1,15]]


[[1,2,3,4,0,0,0],[0,3,2,1,1,0,10],[0,2,5,3,0,1,15]]


