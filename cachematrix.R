makeCacheMatrix <- function(x = matrix()) {
  ## Put comments here that give an overall description of what your
  ## functions do
  
  ## Write a short comment describing this function
  
  ## @x: a square matrix that can be inverted
  ## return: a list containing functions to
  ##              1. set and get the matrix
  ##              2. set and get the inverse of the matrix
  ##         this list is the input to cacheSolve()
  
  invMat = NULL
  set = function(y) {
    # <<- is used  to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    invMat <<- NULL
  }
  get = function() x
  setinv = function(inverse) invMat <<- inverse 
  getinv = function() invMat
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## Write a short comment describing this function
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  invMat = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(invMat)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(invMat)
  }
  
  # else, calculates the inverse 
  mat.data = x$get()
  invMat = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache 
  x$setinv(invMat)
  
  return(invMat)
}
