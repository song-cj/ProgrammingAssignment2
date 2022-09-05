## caching the inverse of a matrix rather than computing it repeatedly

## function 1 makeCacheMatrix
## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # initiate m, x was initiated above defined as a matrix
  m <- NULL
  
  # if x is reset (by set() function outside the makeCacheMatrix) 
  # the m value in the memory will be cleared
  # then the inverse will be recalculate through cacheSolve function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #retrive x value, either from makeCacheMatrix() function input 
  # or from set() outside makeCacheMatrix()
  get <- function() x
  
  #setter of inverse value
  setinv <- function(inv) m <<- inv
  
  #getter of inverse value
  getinv <- function() m
  
  #assign names to list elements
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## function 2 cacheSolve
## computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  #read in inverse value, either NULL or calculated
  m <- x$getinv()
  # if m has already been calculated, the value will be pulled from memory
  # and return a message
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # get the input matrix
  data <- x$get()
  #calculate inverse matrix
  m <- solve(data, ...)
  #pass the recalculated inverse value to the input object
  x$setinv(m)
  #print the inverse matrix
  m
}

## test ##
#test_m1 <- matrix(c(1/2,-1/4,-1,3/4),nrow=2,ncol=2)
#mymatrix <- makeCacheMatrix(test_m1)
#mymatrix$get()
#mymatrix$getinv()
#cacheSolve(mymatrix)
#cacheSolve(mymatrix)
## end test ##