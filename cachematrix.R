## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(ma = matrix()) {
  mi <- NULL
  set <- function(y) {
    ma <<- y
    mi <<- NULL
  }
  get <- function() ma
  setMatrixInverse <- function(inverse) mi <<- inverse
  getMatrixInverse <- function() mi
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## Write a short comment describing this function
matrixInverse <- function(ma){
  mi = solve(ma)
  mi
}

cacheSolve <- function(ma, ...) {
        ## Return a matrix that is the inverse of 'ma'
  mi <- ma$getMatrixInverse()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- ma$get()
  mi <- matrixInverse(data, ...)
  ma$setMatrixInverse(mi)
  mi
}
