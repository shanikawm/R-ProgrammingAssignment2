## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #Setting new matricx
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #get the matrix
  get <- function() 
    x
  #Setting the inverse of the matrix
  setinverse <- function(inverse)
    i <<- inverse
  #Get the invers of the matrix
  getinverse <- function()
    i
  #Return the list with methods
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  #Check whether inverse is already available in the cash
  if (!is.null(i)) {
    #If available return it
    message("getting cached data")
    return(i)
  }
  #If the inverse of the matrix not available get it
  data <- x$get()
  #Calculate the inverse
  i <- solve(data, ...)
  #Set the inverse of the matrix so that next time access from cache
  x$setinverse(i)
  i
}
