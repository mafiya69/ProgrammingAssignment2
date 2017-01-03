## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # inverse of matrix "x"
  set <- function(y) { # set function
    x <<- y
    m <<- NULL
  }
  get <- function() x # get function
  
  setinv <- function(invr) inv <<- invr # set inverse
  getinv <- function() inv # get inverse
  
  list(set = set, get = get, # return list
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv() # get inverse
  if(!is.null(inv)) { # check if "inv" is not null
    message("getting cached data")
    return(inv)
  }
  # if inv is null find inverse
  
  data <- x$get()
  
  # find inverse
  inv <- solve(data)
  
  # set inverse
  x$setinv(inv)
  # return
  inv
}
