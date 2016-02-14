## makeCashMatrix allocates a matrix x 
## cacheSolve depects an inverse of a matrix if it is available,otherwise calculates the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(Inverse) inverse <<- Inverse
  getinverse <- function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve calculates matrix inverse
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting inverse matrix data")
    return(inverse)
  }
  message("inverse is now calculated")
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}