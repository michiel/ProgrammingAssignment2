## Put comments here that give an overall description of what your
## functions do

## Create and return a cacheable matrix object but don't solve anything

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL

  set <- function(y) {
    x    <<- y
    invM <<- NULL
  }

  get <- function() {
    return(x)
  }
  setInvMatrix <- function(i) {
    invM <<- i
  }

  getInvMatrix <- function() {
    return(invM)
  }

  list(set = set,
       get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix
       )

}


## Attempt to retrieve a cached solution from x, 
## if none exists solve it and set the result in the cache with setInvMAtrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invM <- x$getInvMatrix()

  if(!is.null(invM)) {
    message("Cached data found, returning cached data")
    return(invM)

  } else {
    message("No data, solving and setting cached data")
    data <- x$get()
    invM <- solve(data, ...)
    x$setInvMatrix(invM)
    return(invM)
  }
}

## Example test,
# > x =  rbind(c(2, 3), c(3, 2)
# > x
# [,1] [,2]
# [1,]    2    3
# [2,]    3    2
# > cM <- makeCacheMatrix(x)
# > cacheSolve(cM)
# No data, solving and setting cached data
# [,1] [,2]
# [1,] -0.4  0.6
# [2,]  0.6 -0.4
# > cacheSolve(cM)
# Cached data found, returning cached data
# [,1] [,2]
# [1,] -0.4  0.6
# [2,]  0.6 -0.4

