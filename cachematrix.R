## The below functions create a matrix object to store the inverse of the matrix in cache
## and picks the inverse value from cache if the input matrix is same

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(mat) inv <<- solve(mat)
  getinv <- function() inv    
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function computes the inverse of a matrix. If the inverse is already present in the
## cache and the input matrix is the same the inverse is picked up from there else it computes the
## inverse and stores the result in in cache for future reference

cacheSolve <- function(x, ...) {
  inv_matrix <- x$getinv()
  if(!is.null(inv_matrix)) {
    print("getting inverse from cache")
    return(inv_matrix)
  }
  inv_matrix <- solve(x$get())
  x$setinv(inv_matrix)
  inv_matrix
}
