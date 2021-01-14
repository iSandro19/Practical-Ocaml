let isValidMovement m n (x,y) =
  x >= 0 &&
  x <  m &&
  y >= 0 &&
  y <  n;;

let tour m n (xi,yi) (xf,yf) =
  let rec path mov (i,j) =
    