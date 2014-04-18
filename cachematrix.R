## Cache the result of solve in an internal structure along with the matrix


## makeCacheMatrix is a closure that defines the accessors 
## and the mutators of a matrix (x) and its inverse (i)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL	# inverse of the matrix (cache)
  set <- function(y) {
    x <<- y		# assign the new matrix y to x, the internal store of the matrix
    i <<- NULL 	# invalidate the cached inverse of the matrix x
  }
  get <- function() x		# return the value of the matrix x
  setSolved <- function(j) i <<- j # cache the inverse of the matrix x
  getSolved <- function() i # return the value of the inverse
  list(get = get, set = set, getSolved = getSolved, setSolved = setSolved)
}


## Cache the result of solve (ie the inverse of x) into its internal variable i

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getSolved()	# retrieve the cached inverse
  if (!is.null(i)) {
    # the cached inverse has not been invalidated by an update (x$set) of the matrix
    message("getting cached data")
    return (i)
  }
  m <- x$get()	# retrieve the data of the matrix
  i <- solve(m,...)
  x$setSolved(i)
  i
}