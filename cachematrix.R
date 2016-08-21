# makeCacheMatrix is a function which create a list
# containg functions to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of inverse of the matrix
# 4. Get the value of inverse of the matrix

# Input : x
# Output : the list

makeCacheMatrix <- function(x = matrix()) {
  InverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    InverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) InverseMatrix <<- inverse
  getInverse <- function() InverseMatrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

# Input : the list created with MakeCacheMatrix
# Output : x matrix inversed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  InverseMatrix <- x$getInverse()
  if (!is.null(InverseMatrix)) {
    message("getting cached data")
    return(InverseMatrix)
  }
  mat <- x$get()
  InverseMatrix <- solve(mat, ...)
  x$setInverse(InverseMatrix)
  InverseMatrix
}