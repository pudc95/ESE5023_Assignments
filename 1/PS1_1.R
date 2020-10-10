#1
Print_values <- function(a, b, c){
  if(a > b){
    if(b > c){
      sequence  <- c(a, b ,c)
    }
    else{
      if(a > c){
        sequence  <- c(a, c, b)
      }
      else{
        sequence  <- c(c, a, b)
      }
    }
  }
  else{
    if(b > c){
      if(a > c){
        sequence  <- c(a, c, b)
      }
      else{
        sequence  <- c(c, a, b)
      }
    }
    else
      sequence  <- c(c, b, a)
  }
  print(sequence)
}

a = runif(1)
b = runif(1)
c = runif(1)

Print_values(a, b, c)
  
