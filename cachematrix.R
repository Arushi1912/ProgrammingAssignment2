## Below are two functions that are used to create a special object that
## stores a matrix and caches the inverse of the matrix, respectively.

## makeCacheMatrix creates a special function that constitutes a list
## of functions like set, get, setInverse and getInverse. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y 
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve calculates the inverse of the special object created by
## makeCacheMatrix. It first checks if the inverse has already been 
## calculated. If so, it returns the inverse. If not, it continues to 
## evaluate the inverse with the solve() function and sets the inverse
## in the special object accordingly. 

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if (!is.null(i)) {
          message("Getting cached data")
          return(i)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
