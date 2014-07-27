## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cachematrix <-function(x){
  
  ## makeCacheMatrix(x)
}
## The function, makeVector creates a special "matrix",
## which is really a maxtrix containing a function to:
##  1.set the value of the matrix
##  2.get the value of the matrix
##  3.set the value of the inverse
##  4.get the value of the inverse


makeCacheMatrix <- function(x) {
  
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function (solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function checks to see if the inverse of the
## matrix has already been solved and cached.  If so
## it says it's "getting the cached data" and presents
## the result. If not, the result is calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
