module PPATest.Common where

-- Progs

ifBasic :: String
ifBasic =
    "   if (a > 0 || a < 0) then \
      \     (x := -2)            \
      \ else                     \
      \     (skip)"

ifTwice :: String
ifTwice = 
    "   if (a > 0 || a < 0) then \ 
      \     (x := -2)            \ 
      \ else                     \ 
      \     (skip);              \ 
      \ if (b < 5) then          \
      \     (y := 1)             \
      \ else                     \
      \     (skip)"

{- y^x
 - result stored in z -}
power :: String
power = 
    "   z := 1;             \
      \ while (x > 0) do    \
      \     (z := z * y;    \
      \      x := x - 1)"

{- x!
 - result stored in z -}
factorial :: String
factorial = 
    "   y := x;             \
      \ z := 1;             \
      \ while (y > 1) do    \
      \     (z := z * y;    \
      \      y := y - 1);   \
      \ y := 0"


