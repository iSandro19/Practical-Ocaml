let isValidMovement m n (x,y) =
  x >= 0 &&
  x < m  &&
  y >= 0 &&
  y < n;;

let tour m n (xi,yi) (xf,yf) =
  let rec path mov (i,j) =
    if i > m then [mov]
    else if j > n then []
    else if compatible (i,j) mov
      then match path ((i,j)::mov) (i+1,1) with
          [] -> path mov (i,j+1)
        | solution -> solution
    else path mov (i,j+1)
  in path [] (1,1);;