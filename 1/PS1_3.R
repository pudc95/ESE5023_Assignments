Pascal_triangle <- function(k){
  Pas <- c()
  t <- 1 
  for (i in 1:k) {
      t <- factorial(k - 1)/(factorial(i - 1) * factorial(k - i))
      Pas[i] <- t
  }
  return(Pas)
}
Pascal_triangle(100)
Pascal_triangle(200)
