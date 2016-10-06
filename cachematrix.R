## This set of function is to simply the matrix inverse calculation by 
## creating a list that contains the original matrix and its inverse
## which will be calcualted and cached in the first attempt to calculate
## the inverse. And such cached result will be returned in later calls
## for the calcuation of inverse for this matrix to save time

## the makeCacheMatrix function takes a matrix object x and returns a list
## containing four functions (set, get, setinverse and getinverse), which
## sets the matrix, return the matrix value, set the value of inverse of 
## the matrix and return the inverse of matrix, respectively. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## initilize pointer that stores the inverse
  set <- function (y) { ## set the initial value
    x <<- y  ## set the x  parent environment to be y
    m <<- NULL ## set the m in in parent enviroment to be NULL
  }
  get <- function() x  ## get matrix from current enfironment
  setinverse <- function(slove) m <<-slove ## get m from solve function
  getinverse <- function() m ## get m from current environment 
  list (set = set, get = get, ## return the list
        setinverse = setinverse,
        getinverse = getinverse)
}


## The cacheSolve function take a list created by makeCacheMatrix and
## and parameter for solve function. If no cached inverse for the matrix
## is avaliable, the inverse will be calculated, stored in the list and
## returned, while if a cached inverse is avalaible, the inverse will be
## returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() ## get cached inverse
  if(!is.null(m)){  ## if the cached inverse is avaliable
    message("getting cached data") ## send message
    return(m) ## return the cached inverse
  }
  data <- x$get()  ## get the matrix data
  m <- solve(data, ...)  ## calculate the inverse
  x$setinverse(m)  ## cache the inverse
  m  ## return the inverse
}