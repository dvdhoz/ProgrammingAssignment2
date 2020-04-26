
## This function calculates the matrix inverse.  
## It will first check to see if the inverse has been calculated 
## and will skip the computation.  If computation has not been computed, 
## then the inverse will be calculated

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setMatrix <- function(inverse) m <<- inverse
  getMatrix <- function()m
  list(set = set, get = get,
       setMatrix=setMatrix,
       getMatrix=getMatrix)
}


## This function computes the inverse of the matrix created by 
## makeCacheMatrix above. If the inverse has already been calculated, 
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  ## Return a matrix that is the inverse of 'x'
}
