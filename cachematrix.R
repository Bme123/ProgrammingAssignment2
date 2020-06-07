#makeCacheMatrix is a function that gets the matrix from the user and sets it.
#it also gets the inverse function and sets it
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
#cacheSolve function is used to cache the matrix by first checking if the matrix has already been used
#if it is not, it first caches it and then calculates the inverse by using solve function
#if inverse previously set has been entered, the inverse is simply get from the cache rather than making the calculation
#time saving approach
cacheSolve<-function(x,...){
  inv<-x$getInverse()
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat, ...)
  x$setInverse(inv)
  inv
}
