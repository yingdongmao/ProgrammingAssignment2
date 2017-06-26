## the calculation of the value of the inverse matrix would always be
## time consuming. The following two functions could cache the 
## inverse of matrix

## makeCacheMatrix function create a list containing:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) v <<- inverse
  getinverse <- function() v
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The following function returns the reverse of the matrix.

cacheSolve <- function(x, ...) {
  v <- x$getinverse()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)
  x$setinverse(v)
  v  
}
