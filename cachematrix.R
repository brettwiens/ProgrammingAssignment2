## Creates a function called "makeCacheMatrix" that takes a matrix (extX)
## and caches its inverse.

makeCacheMatrix <- function(extX = matrix()){
  inverse_matrixX <- NULL
  ## NULLs the inverse_matrixX object in the function environment.
  
  ## set is a function that takes a variable (y) and stores that value into the parent
  ## environment in extX (I use ext to indicate that the variable is stored externally
  ## to the function environment.  It also NULLs the inverse_matrixX variable in the
  ## parent environment.
  set <- function(y) {
    extX <<- y
    inverse_matrixX <<- NULL
  }
 
  ## get is a function that simply pulls the extX value from the parent environment and
  ## stores it as get.  extX might as well be in {} but in this case it fits nicely in-line
  get <- function() extX
  
  ## setInverse takes an input object (the inverse matrix named "inverse" and stores it
  ## externally to the parent environment as the formerly NULLed inverse_matrixX
  setInverse <- function(inverse) inverse_matrixX <<- inverse
  
  ## getInverse, as with get, just pulls the value of inverse_matrixX from the parent
  ## environment and stores it as getInverse in the function environment
  getInverse <- function() inverse_matrixX
  
  list (set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}

## calculates the inverse of a matrix (extX) that was created 
## in makeCacheMatrix.  If that matrix exists already, it just calls the
## inverse that is cached (saving processing time.)

cacheSolve <- function(extX, ...){
  inverse_matrixX <- extX$getInverse()
  ## stores the value from getInverse to inverse_matrixX (function environment)
  
  ## If the inverse_matrixX is not NULL, that would indicate that it has already
  ## been calculated and cached, in this case, alert the user and return that 
  ## inverse_matrixX
  if(!is.null(inverse_matrixX)){
    message("getting cached data")
    return(inverse_matrixX)
  }
  
  ## having discovered in the previous step that inverse_matrixX IS NULL, and therefore
  ## that it has neither been calculated nor cached, we need to create and cache it.
  data <- extX$get()
  ## the get function is called using the extX value, and returns the matrix the get call
  inverse_matrixX <- solve(data)
  ## the inverse matrix is calculated using the solve function.  Since the second argument
  ## (b) is missing, it is identified to be an identity matrix, and the inverse of data is
  ## returned.
  extX$setInverse(inverse_matrixX)
  ## The setInverse function is called, which stores the inverse_matrixX into the parent
  ## environment. 
  inverse_matrixX
  ## cacheSolve	returns the inverse matrix.
}
