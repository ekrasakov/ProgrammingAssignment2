makeCacheMatrix <- function(x = matrix()) {
  # The cached inverse of a given matrix
  m <- NULL
   # getting and setting the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
   # getting and setting the inverse
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
   # Return a list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  # Return the inverse if its already been computed
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } 
   # compute the inverse
    data <- x$get()
    m <- solve(data, ...)
   # cache the inverse
    x$setInverse(m)
   # cache the inverse
    m	
}
