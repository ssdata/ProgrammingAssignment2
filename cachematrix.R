## The following function creates the special matrix that can be used
## by the special solve function below.
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  # to set the value so that the context is this matrix
  set <- function(y) {
    x <<- y # so that the parent variable that hold the matrix is assigned to
    inv <<- NULL   # since the matrix changed and the parent variable that holds the inverse should be reset
  }

  get <- function() x
  setinv <- function(inverse) inv <<- inverse # the parent variable that hold the inverse should be changed.
  getinv <- function() inv
  
  # return a list of all the above functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)    
}


## The following function calculates the inverse of the matrix
## created by above method and puts into the cache. When reading
## it first checks if availabe in cache and only then computes.

cacheSolve <- function(x) {
  # check if the cache has the inverse
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  # compute and put into cache
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}