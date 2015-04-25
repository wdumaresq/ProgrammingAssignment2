## The two functions makeCacheMatrix and cacheSolve may be used together
## in order to compute and cache the inverse of a matrix so that the
## actual computation only needs to happen once for any given matrix.

## Usage example:
## > x <- makeCacheMatrix(matrix(1:4, 2, 2))
## > cacheSolve(x)

## The first call to cacheSolve(x) will compute and return the
## inverse of the matrix. Subequent calls will simply return
## the cached inverse.


## makeCacheMatrix - This function returns a list of 4 functions
##    (set, get, setinverse, getinverse) all referencing an
##    environment which contains storage for a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve - This function accepts a list that was
##       returned from makeCacheMatrix and either computes
##       the inverse of the matrix stored within the list's
##       environment and caches and returns this inverse, or returns the
##       cached inverse from the environment.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}  

