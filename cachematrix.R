## The pair of functions implement caching the inverse of a matrix.
## This is useful when using an inverse matrix multiple times within
## an algorithm, especially on large matrices whose inverse computation
## is expensive.  The inverse will be calculated once and then cached,
## so when the inverse is requested again, it will simply be fetched.

## Creates a container object to hold a matrix. Uses a getter/setter pattern.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Compute the inverse matrix, unless it was already computed and cached.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
