## Calculates and caches the inverse of a invertible, square matrix. 

## Creates a special "matrix".

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
get <- function() x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(set = set,
     get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## Finds the inverse of the matrix created with first function, while
## checking to see if the inverse has already been calculated.

cacheSolve <- function(x, ...) {  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
