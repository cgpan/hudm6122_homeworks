library("MVA")
demo("Ch-MDS")

chi_squared_dist_matrices <- function(tbl){
  # Calculate row sums and column sums
  row_sums <- apply(tbl, 1, sum)
  col_sums <- apply(tbl, 2, sum)
  # Calculate total count and expected counts
  total_count <- sum(tbl)
  
  # to have the dim of the input matrix
  c <- ncol(tbl)
  r <- nrow(tbl)
  # make a empty matrix to load all elements
  col_d_matrix <- matrix(0,c,c)
  
  # using loop to write the each ij entries into this matrix
  for (i in c(1:c)) {
    for (j in c(1:c)) {
      d_ij <- 0
      for (k in c(1:r)) {
        p_k_dot <- row_sums[k]/total_count
        p_k_i <- tbl[k,i]/col_sums[i]
        p_k_j <- tbl[k,j]/col_sums[j]
        d_ij <- d_ij + (1/p_k_dot)*(p_k_i-p_k_j)^2
      }
      col_d_matrix[i,j] <- d_ij
    }
  }
  # make a empty matrix to load all elements
  row_d_matrix <- matrix(0,r,r)
  
  # using loop to write the each ij entries into this matrix
  for (i in c(1:r)) {
    for (j in c(1:r)) {
      d_ij <- 0
      for (k in c(1:r)) {
        p_dot_k <- col_sums[k]/total_count
        p_i_k <- tbl[i,k]/row_sums[i]
        p_j_k <- tbl[j,k]/row_sums[j]
        d_ij <- d_ij + (1/p_dot_k)*(p_i_k-p_j_k)^2
      }
      row_d_matrix[i,j] <- d_ij
    }
  }
  return(list(col_d_matrix, row_d_matrix))
}

chi_squared_dist_matrices(tbl)