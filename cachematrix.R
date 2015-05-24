
# make and store a (cache) matrix 
# then hold the cached value or NULL if nothing is cached
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get/return a stored matrix and cache values
  getMatrix <- function() x
  setinversed <- function(inverse) m <<- inverse 
  getinversed <- function() m
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setinversed = setinversed,
       getinversed = getinversed)
}

# solve for the inverse of the matrix above
cachesolve <- function(x, ...) {
  
  # get the value that was cached 
  inv <- x$getinverse()    
  if(!is.null(inv)) {
    message("Getting Cached Data")
    return(inv)     
  }
  data <- x$get()
  inv <- solve(data)  
  x$setinverse(inv)
  # returns the inverse
  inv
}
