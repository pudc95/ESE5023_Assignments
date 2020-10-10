#2.1
M1 <- matrix(sample(1:50, 50),nrow = 5,ncol = 10,byrow = T)
M2 <- matrix(sample(1:50, 50),nrow = 10,ncol = 5,byrow = T)

#2.2
Matrix_multip <- function(M1, M2){
  M1_row_col <- dim(M1)
  M2_row_col <- dim(M2)
  if(M1_row_col[2] == M2_row_col[1]){
    multip <- matrix(nrow=M1_row_col[1],ncol=M2_row_col[2])
    for (i in 1:M1_row_col[1]) {
      for (j in 1:M1_row_col[1]) {
        S <- M1[i,]*M2[,j]
        Sum <- 0
        for (k in 1:M1_row_col[2]){
          Sum <- Sum+S[k]
        }
        multip[i,j] <- Sum
      }
    }
    return(multip)
  }
}

multip <- Matrix_multip(M1, M2) 
M3 <- M1 %*% M2        
