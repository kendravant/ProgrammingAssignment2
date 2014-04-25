## Two functions to allow the caching of the calculated matrix inverse to
## avoid computationally costly recomputation


makeCacheMatrix <- function(mat = numeric()) {
  ## Creates a CacheMatrix object that stores the computation of a matrix inverse 
  ## for later reuse. 
  m <- NULL
  set <- function(y) {
    mat <<- y
    m <<- NULL
  }
  get <- function() mat
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Checks to see if a cached version of the matrix exists. Returned cached value
  ## if exists. Otherwise calculates the matrix inverse.
  m <- x$getInverse()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
