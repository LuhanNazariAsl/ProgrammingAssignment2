## this function takes a matrix, takes its inverse and caches it  into memory
## so it can be recycled if needed,fast

## this  function takes a matrix,finds the inverse and 
##provides the list of items needed for recalling it
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## this func checks to see if the inverse  is available in memory if not finds it

cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
