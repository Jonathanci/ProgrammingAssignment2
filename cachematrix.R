
## The makeCacheMatrix function takes a matrix as input
## and resets the cache holding the result of the inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  
  #Set matrix value and clear cache.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Get the value of the matrix
  get <- function() {
	x
  }
  
  #Set inverse. used by getinverse only when there is no cached inverse.
  setinverse <- function(inverse) {
	m <<- inverse
  }
  
  #Get the inverse
  getinverse <- function() {
	m
  }
  
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}

## Function cacheSolve calculates the inverse of the matrix 
## by first checking if the result already exists (m), and 
## returns the existing result, and recalculates the inverse 
## if it does not exists.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(is.null(m)) {
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
  } else {
	 message("getting cached data")
  }
  m
}
