## The functions below efficiently determine the inverse of a matrix
## If the inverse has previously been found "cached" version is returned
## This avoids the need to recalculate the inverse.

## The following function creates a list object containing 4 functions:
## 1. set the value of the matrix whose inverse is to be found
## 2. get the matrix in 1.
## 3. set the value of the inverse of the matrix 
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set =set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function, makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
