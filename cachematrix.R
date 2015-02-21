#It provides four functions- create matrix, retrieve matrix, 
# create inverse of matrix and retrieve inverse of matrix.
# This function takes a matrix as an input. If no input is provided, a default matrix with NA as value will be used.
makeCacheMatrix <- function(x = matrix()) {
# Initializing inversion matrix to null.
  inv <- NULL
# Sets matrix y to x( in a different environment) and inversion matrix to null.  
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  getMatrix <- function() x
  
# Sets inversion matrix. Assigns z to inv( in a different environment)  
  setInvMatrix <- function(z) inv <<- z
  getInvMatrix <- function() inv
  
#List of functions is created and returned.  
  list(setMatrix = setMatrix, getMatrix = getMatrix ,
       setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)

}


# It provides inverse of a matrix. The inversion is looked up in the cache.
# If there is no data in the cache, it is computed, put into the cache and returned.
# This function takes list of functions and variable number of arguments
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInvMatrix()
  
  #Checks if cache is set with inversion matrix. If yes, return the cache value.
  if(!is.null(invMatrix)) {
    print("Obtained the inverse of matrix from cache")
    return(invMatrix)
  }
  
  #Else, compute the inversion and set it in the cache and return the value.
  data <- x$getMatrix()
  invMatrix <- solve(data)
  x$setInvMatrix(invMatrix)
  invMatrix
}