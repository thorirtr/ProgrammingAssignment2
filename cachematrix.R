## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a matrix from a vector of numbers while cacheSolve 
## creates an inverse of the matrix and then returns that inverse to makeCacheMatrix

## Write a short comment describing this function
## makeCacheMatrix takes a vector of numbers and creates a matrix out of them.
## That is if the vector can be squared (e.g. 5x5 or 4x4).
## It also checks if the vector has changed since last time, checks if there is 
## already a matrix made out of the vector. The function also checks if the matrix
## is invertible prior to calling cacheSolve to invert the matrix. Finally it prints
## the matrix and its inverse as well as notifying if the inverse was fetched from 
## a cache or calculated. 
makeCacheMatrix <- function(vect = matrix()) {
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
##}


## Write a short comment describing this function
## cacheSolve takes a matrix from makeCacheMatrix and inverses it, then returns the inverse back.

cacheSolve <- function(data)
  {
    inversed <- solve(data)
    return(inversed)
  }