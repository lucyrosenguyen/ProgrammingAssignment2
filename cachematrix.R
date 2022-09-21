## Caching the Inverse of a Matrix: Both makeCacheMatrix & cachesolve functions 
## are used to create a special object that stores a matrix and caches its 
## inverse to eliminate the need to cache potentially time-consuming computations.

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #Initializing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x #Function to get matrix x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv #Function to obtain inverse of the matrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) { #Gets cache data
  inv <- x$getInverse() #Checking whether inverse is NULL
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv) #Returns inverse value
  }
  data <- x$get()
  inv <- solve(data, ...) #Calculates inverse value
  x$setInverse(inv)
  inv ##Return a matrix that is the inverse of 'x'
}