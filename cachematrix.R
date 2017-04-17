## These two functions together compute and return the inverse of a matrix input. 
## If the matrix input has already been calculated once and the returned value been saved in the associated object of type makeCacheMatrix,
## the functions overide the calculation and return the saved value instantly.

## This function stores the matrix created or save the inverse matrix function cacheSolve calculated when the original matrix is not changed.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat_invs <- function(mat_invs) m <<- mat_invs
  getmat_invs <- function() m
  list(set = set, get = get,
       setmat_invs = setmat_invs,
       getmat_invs = getmat_invs)

}


## This function computes the inverse of the matrix returned by makeCacheMatrix above 
## or return the inverse matrix saved in makeCacheMatrix if the original matrix has already been computed. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmat_invs()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmat_invs(m)
  m
}
