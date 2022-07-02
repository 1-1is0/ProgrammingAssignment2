## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix is responsibile for creating the matrix and saving the matrix inverse
## cacheSolve check the object from makeCacheMatrix and try to get the inverse value from memory
## if it is not calculated before it find and save it in memory for feature uses


## Write a short comment describing this function
## with this fucntin we create a matrix x and save its inverse in `inv` variable
## if the metrix change at anymoment the`set` fucntion would NULL its invese and force to 
## calculate it when its needed. 
## `get` just returh the same matrix x
## `setinv` set the value for matrix invese so we have it somewhere in memory
## `getinv` retrive the inv value for us from memory, if the value is NULL we findout it
## has never been calculated before so we calculate it and save it in the makeCacheMatrix.inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i 
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
