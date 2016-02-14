## Put comments here that give an overall description of what your
## functions do

## makeCashMatrix allocates a matrix x 
## cacheSolve depects an inverse of a matrix if it is available,otherwise calculates the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(Inverse) inv <<- Inverse
  getinv <- function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## cacheSolve calculates matrix inverse
cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inv)){
    message("getting inverse matrix data")
    return(inv)
  }
  message("inverse is now calculated")
data <- x$get()
inv <- solve(data, ...)
x$setinv(inv)
inv
}