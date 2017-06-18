## Functions to create matrix with a cached inverse to improve performance when reusing the inverse in code

## makeCacheMatrix creates a list of functions that can be used to get and set a matrix and its inverse
## To use:
## Create a matrix: m <- matrix(rnorm(25), 5, 5)
## Construct the cacheMatrix: cm <- makeCacheMatrix(m)
## Get the matrix: m <- cm$get()
## Get the inverse: inv <- cm$getinverse()
## Set the matrix: cm$set(m)
## Set the inverse: cm$setinverse(inv)

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  getinverse <- function() i
  setinverse <- function(inv) i <<- inv 
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


## Gets the inverse of a matrix, 
## if the inverse has not been calculated before it calculates and caches it
## if the inverse has already been calculated it returns the cached inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m)
  x$setinverse(inv)
  inv
}
