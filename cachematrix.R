## These functions allow you to input a matrix and set its inverse
##makeCacheMatrix creates a special list of functions with a matirx
## to set the value of a matrix, get the value of the matrix
## set the value of the inverse of the matrix, and get the value 
##of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  print(environment())
  evn <- environment()
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  getevn<- function() environment()
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       getevn = getevn)
}
## Write a short comment describing this function
##This function calculates and caches the inverse of x
##unless the inverse is already calculated, then it returns 
##the inverse where x is an output of the makeCacheMatrix function
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
  ## Return a matrix that is the inverse of 'x'
