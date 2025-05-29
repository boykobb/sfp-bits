import Data.List

type Vec = [Double]                     -- точка или вектор с каква да е размерност
data Obj = Poly [Vec] | Comp [OPlaced]  -- „обект“: прост – начупена линия – или множество от „разположени обекти“
type OPlaced = ([Vec],Obj)              -- поставен обект: обект с преобразуваща матрица

-- Умножаване на матрици: всеки ред на ass се умножава с редовете (не стълбовете!) на bss.
mprod ass bss = [[sum $ zipWith (*) xs ys | ys <- bss] | xs <- ass]

-- Разполагане на обект (за чертане).
-- Резултатът е списък от начупени линии с върхове в координати за чертане.
drawingList (m, Poly ps) = [transpose $ mprod m ps]
drawingList (m, Comp os) =
  concatMap (\(m',o) -> drawingList (mprod m (transpose m'), o)) os

-- Пример:
o = ([[1.0,2],[3,4]], Comp [([[5.0,6],[7,8]], Poly [[0,0],[1,0],[1,1],[0,1]])])
r = drawingList o
