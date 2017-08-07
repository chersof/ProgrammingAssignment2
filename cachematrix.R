## makeCacheMatrix creates a special matrix object, and cacheSolve
## calculates the inverse of this matrix.
## If the matrix inverse has already been calculated, it will instead
## find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y   
    m <<- NULL 
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of makeCacheMatrix. if its has already
## been calculated the cachesolve retrieves it from the cache.

cacheSolve <- function(x) {
  m <- x$getInverse()
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data) 
  x$setInverse(m)  
  m              
}