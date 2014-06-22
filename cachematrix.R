## makeCacheMatrix
## This function returns a list that contains a matrix and its inverse
## The purpose is to be able to cache the matrix inverse to speed up computation

makeCacheMatrix <- function(x = matrix()) {
  ## Set m to null
  m <- NULL
  ## Create local function set, which sets x to y and m to null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Create local function get
  get <- function() x
  ## Create local function setsolve
  setsolve <- function(solve) m <<- solve
  ## Create local function getsolve
  getsolve <- function() m
  ## Return a list of functions to get and set matrix and inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve
## This function computes and stores the inverse for the matrix lists created by makeCacheMatrix
## Inverse is taken from the cache if such exists

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