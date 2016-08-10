## Caching the inverse of a matrix

## This function will create matrix object that is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverseM <- NULL
    set <- function(y) {
      x <<- y
      inverseM <<- NULL
    }
    get <- function() x
    setMatInv <- function(invMatrix) inverseM <<- invMatrix
    getMatInv <- function() inverseM
    list(set = set,
         get = get,
         setMatInv = setMatInv,
         getMatInv = getMatInv)
}


## This function will output the inverse of the matrix created in the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverseM <- x$getMatInv()
    if (!is.null(inverseM)) {
      return(inverseM)
    }
    myMat <- x$get()
    inverseM <- solve(myMat, ...)
    x$setMatInv(inverseM)
    inverseM
}
