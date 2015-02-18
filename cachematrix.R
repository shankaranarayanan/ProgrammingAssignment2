#It provides four functions- create matrix, retrieve matrix, 
# create inverse of matrix and retrieve inverse of matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  getMatrix <- function() x
  setInvMatrix <- function(z) inv <<- z
  getInvMatrix <- function() inv
  
  list(setMatrix = setMatrix, getMatrix = getMatrix ,
       setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)

}


# It provides inverse of a matrix. The inversion is looked up in the cache.
# If there is no data in the cache, it is computed, put into the cache and returned.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInvMatrix()
  if(!is.null(invMatrix)) {
    print("Obtained the inverse of matrix from cache")
    return(invMatrix)
  }
  
  data <- x$getMatrix()
  invMatrix <- solve(data)
  x$setInvMatrix(invMatrix)
  invMatrix
}