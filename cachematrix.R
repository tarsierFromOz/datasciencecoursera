## The folowing functions makeCacheMatrix and cacheSolve are used to 
## create and invert matrix respectively. The inverse of the matrix
## which was solved is stored and can be retrieved from the cache


## The function below has the capability to create and return 
## a list of functions that can be utilised by the succeding function
## (cacheSolve) which is used in getting and setting the inverted
## matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
  
    #initialised the cache value to null
    inverseValue <- NULL
    
    # working environment creation for the matrix
    set <- function(y) {
      x <<- y
      inverseValue <<- NULL
    }
  
    # retrieves matrix value
    get <- function() x
    
    # store to cache the inverted matrix
    setInverse <- function(inverse) inverseValue <<- inverse
    
    # retrieve the inverted matrix from cache
    getInverse <- function() inverseValue
    
    # list of functions to be provided the working environment
    list(set = set,get = get,setInverse = setInverse, getInverse = getInverse)
}




## This function performs calculation of the inverse matrix created
## in the prior function (makeCacheMtrix)

cacheSolve <- function(x, ...) {
  
  #tries to retrieve the inverse of matrix from the cache
  inverseValue <- x$getInverse()
  
  # test if the cache has contents the returns its contents if the condition provided is true
  if (!is.null(inverseValue)) {
    message("retrieving cache...")
    return(inverseValue)
  }
  
  # this will be performed if matrix is not existing
  m <- x$get()
  inverseValue <- solve(m, ...)
  x$setInverse(inverseValue)
  inverseValue
}
