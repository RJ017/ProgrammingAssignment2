## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  #initialisation
  m <- NULL
  #defining components of matrix object
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  #returning list (matrix object)
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #fetching value of inverse
  m <- x$getinv()
  #checking if matrix change else fetch from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##fetching value
  data <- x$get()
  #calculating
  m <- solve(data, ...)
  x$setinv(m)
  #returning answer
  m
}
