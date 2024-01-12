## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function consists of set, get, setinv, getinv
# The package MASS here is called to calculate inverse for sqaured and non squared matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x # function to get x matrix
  setinv <- function(inverse)inv <<- inverse
  getinv <- function() {
    inver<- ginv(x)
    inver%*%x # function to obtain the inverse matrix
  }
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## Write a short comment describing this function
# This is used to get the cache data
cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)) {
    message("getting cached data!") 
    return(inv)                     # returns the inverse value
  }
  data <- x$get()
  inv <- solve(data, ...)  # calculates inverse value
  x$setinv(inv)
  inv         # return a matrix that is the inverse of x
  
}

