## Put comments here that give an overall description of what your
## The first function creates a vector of functions that can be applied to a given dataframe.  The second function 
##  does that - passes dataframe to a specific function from the vector of functions created in the first function.  

## The following function creates a vector of functions - they are get, set - getter and setter for matrix
## and getInv, setInv - getter and setter for inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(matrix) m <<- matrix
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## First checks if the inverse exists for a given matrix.  If so, returns that.  
## Else, computes, caches and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
