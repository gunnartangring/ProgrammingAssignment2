## amatrix <- matrix(c(1, 2, 3, 4), nrow = 2,  ncol = 2)
##  x<-matrix(c(2,4,1,3),2,2)

## makeCacheMatrix
## This function returns a list that contains a matrix and its inverse
## The purpose is to be able to cache the matrix inverse to speed up computation
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  ## Return a list of functions to get and set matrix and inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve
## This function computes and stores the inverse for the matrix lists vreated by makeCacheMatrix
##
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Get inverse for matrix
  m <- x$getsolve()
  ## Check if there is a stored inverse for the matrix. Note that a changed matrix results in the inverse set to null
  if(!is.null(m)) {
    message("getting cached data")
    ## Return the inverse from the cache and break
    return(m)
  }
  ## Get the matrix
  data <- x$get()
  ## Compute the inverse of the matrix
  m <- solve(data, ...)
  ## Store the inverse in the matrix list
  x$setsolve(m)
  ## Return the inverse
  m
}
