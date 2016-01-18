## Function to cache the inverse of a matrix

## The first function, `makeCacheMatrix` creates a special "Matrix",
## which is a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

  makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
  }


## This function computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), 
## then `cacheSolve` should retrieve the inverse from the cache.

  cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getInverse()
          if (!is.null(inv)) {
                  message("getting cached data")
                  return(inv)
          }
          data <- x$get()
          inv <- solve(data, ...)
          x$setInverse(inv)
          inv
  }
