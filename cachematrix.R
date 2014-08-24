## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #check if vector can be squared, e.g. vector with 25 values, makes 5x5 matrix
  if (all.equal(sqrt(length(vect)),as.integer(sqrt(length(vect)))) == TRUE) {
    # Check if there is older matrix available, if variable doesn't exist 
    # (first time), then create empty matrix of same size for comparison
    if (exists("old")==FALSE) {
      old <- matrix(,sqrt(length(vect)),sqrt(length(vect)))
    }
    #Create matrix from input vector and enter value into varible M
    M <- matrix(vect,nrow=sqrt(length(vect)),ncol=sqrt(length(vect)))
    # Check if it is possible to inverse the matrix, if not break and exit
    if(det(M) == 0) {
      return(message ("Matrix is not invertible, try again with different vector"))
    }
    
    ## Check if matrix has changed, if it has (!identical), call the cacheSolve() 
    ## function and calculate new inverse and set inverse variable
    if (!identical(M,old)){
      message("New matrix, calculating inverse of matrix, please hold...")
      inverse <<- cacheSolve(M)
      # Set the new value of M to the old variable for later comparison purposes
      old <<- M
    }
    else 
    {
      message("Fetching inverse from cache")
    }
    message("Matrix:")
    print(M)
    message("Inversion of matrix:")
    print(inverse)
  }
  else {
    # Error handling and messaging to user
    message("Error: Length of vector cannot be squared, e.g. 3x3, 4x4. Make sure length is 4, 9, 16, 25, etc.")
    message("Use call like: makeCacheMatrix(c(3,4,5,6,7,8,3,1,5)")
  }
}
}


## Write a short comment describing this function
## cacheSolve takes a matrix from makeCacheMatrix and inverses it, then returns the inverse back.

cacheSolve <- function(data)
  {
    inversed <- solve(data)
    return(inversed)
  }