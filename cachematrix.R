## These are functions for creating a matrix object that has methods for 
##initializing the matrix, returning the value of the matrix, and for initializing
##and returning the matrix inverse.

## This function creates the matrix object with the following methods:
## set: This initializes the matrix X with the argument Y.
## get: This returns the value of the matrix.
## setivrs: This initializes the matrix inverse with argument ivrs.
## getivrs: This returns the value of the matrix inverse.

makeCacheMatrix <- function(X = matrix()) {
  Xivrs <- NULL
  set <- function(Y) {
    X <<- Y
    Xivrs <<- NULL
  }
  get <- function() X
  setivrs <- function(ivrs) Xivrs <<- ivrs
  getivrs <- function() Xivrs
  list(set = set, get = get,
       setivrs = setivrs,
       getivrs = getivrs)
}


## This matrix first checks to see if the matrix inverse has been computed and stored.
## If it has, it retrieves the inverse from the cache.  If it has not, it computes and
## stores it in the cache.

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'x'
  Xivrs <- X$getivrs()
  if(!is.null(Xivrs)){
    message("getting cached inverse")
    invisible(Xivrs)
  }
  data <- X$get()
  Xivrs <- solve(data)
  X$setivrs(Xivrs)
}
