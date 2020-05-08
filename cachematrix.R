## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix produces a list of functions and returns it
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  
  list(set = set, get = get, getinv = getinv, setinv = setinv)

}


## Write a short comment describing this function
## cacheSolve finds the inverse and uses set to assign the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached inverse/data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
