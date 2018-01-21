#first part

makeCacheMatrix <- function(x = matrix()) {
  #we create the original matrix
  inv = NULL
  set = function(y) {
    x <<- y  #we assign the value to x
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

#we compure the inverse of the matrix
cacheSolve <- function(x, ...) {
  inv = x$getinv()
 
  if (!is.null(inv)){
    #returns data only if they are not NULL
    message("Getting cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
 # we set the value of the inverted matrix
  x$setinv(inv)
  
  return(inv)
}