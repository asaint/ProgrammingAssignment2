## Two functions defined in this R script file, makeCacheMatrix and cacheSolve, create
## the inverese of a matrix and return it to the calling function. The inverse is
## computed only if required. If the original matrix has not changed, a cahced copy of
## its inverse is returned.

## makeCacheMatrix is a function that manages caching of the inverse of a matrix.
## It returns a vector which is a list containing functions that set and get the
## original matrix object, as well as functions that calculate and return the inverse
## of the original matrix. The inverse is only calculated if required.
makeCacheMatrix <- function(x = matrix()) {
    ## set the inverse object to be NULL at the beginiing.
    inv <- NULL
  
    ## set function assigns a matrix to be inversed to a variable x, and resets the
    ## inv flag indicating that the previously cached data cannot be used.
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
  
    ## get function returns the matrix to be inversed
    get <- function() x
  
    ## setinv funciton calculates the inverse of the matrix and stores it in the
    ## variable called inv
    setinv <- function(solve) inv <<- solve
  
    ## getinv returns the inveresed matrix object inv
    getinv <- function() inv
  
    ## A list of the functions is returned to the caller
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## cachedSolve is a function that takes a matrix as an argument and returns its
## inverse. It utilizes the makeCacheMatrix function to cache a copy of the inverse
## of the matrix object. The cached copy is used if the original matrix did not change,
## Otherwise a newly computed inverse is returned.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    ## Get the value of cached inverse
    m <- x$getinv()
    
    ## If the inverese is already cached, then return the cached value
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    ## If the cached value of inverse does not exist, compute it first
    data <- x$get()
    inv <- solve(data, ...)
    
    ## Now cache the inverse matrix for future use
    x$setinv(inv)
    
    ## return the newly computed inverse
    inv
}


## Test function to test caching of matrix inverse
testInverse <- function(mat = matrix()){
  mat <- cbind(c(1,2), c(3,4))
  
  ## First call, inverse needs to be computed
  cachedMat <- makeCacheMatrix(mat)
  print(cacheSolve(cachedMat))
  
  ## Repeat call using the same matrix, cached value used
  print(cacheSolve(cachedMat))
  
  ## reset the original matrix, inverse will be re-computed
  cachedMat$set(cbind(c(1,3),c(2,4)))
  print(cacheSolve(cachedMat))
}
