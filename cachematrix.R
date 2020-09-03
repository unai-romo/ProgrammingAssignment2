## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) { # Set the value of the matrix
            x <<- y
            i <<- NULL
      }
      get <- function() x # Get the value of the matrix
      setInverse <- function(inverse) i <<- inverse # Set the value of the inverse
      getInverse <- function() i # Get the value of the inverse
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Using makeCacheMatrix, if the inverse has been calculated,
## it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        i <- x$getInverse() # Tries to get the inverse
        if(!is.null(i)) { # If the inverse has been calculated it gets retrieved from the cache and we avoid further calculation
              message("getting cached data")
              return(i)
        } # If the inverse has not been calculated we start the computation to get it
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
