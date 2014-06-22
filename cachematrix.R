## Put comments here that give an overall description of what your
## functions do

##THIS 2 FUNCTION HELP IN CACHING THE INVERSE OF A MATRIX IN CASE OF HEAVY COMPUTATION
## EACH FUNCTION HAVE A SHORT COMMENT EXPLAINING THE PURPOUSE
##BELOW EXAMPLE OF THE USE OF THE 2 FUNCTION IN R:

## EXAMPLE USE:
## matrixdata <- makeCacheMatrix()
## matrixdata$set(matrix(c(1,2,3,4),nrow=2,ncol=2))
## matrixdata$get()
## cacheSolve(matrixdata)


## Write a short comment describing this function
##function thta create an adhoc CacheMatrix object with 4 methods(set,get,setinv,getinv)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) {
    m <<- inv 
  }
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
##function that compute the inverseOF X only if not already cached in the x object
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}

