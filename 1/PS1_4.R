Least_moves <- function(k,n = 0){
  if(k == 1){
    return(n)
  }
  if(k %% 2 == 0){
    n = n + 1
    k = k / 2
    return(Least_moves(k,n))
  }
  else{
    n = n + 1
    k = k - 1
    return(Least_moves(k,n))
  }
}
Least_moves(5)
Least_moves(3)
