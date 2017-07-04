## The functions cache the inverse of a matrix
## The function below creates a matrix that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(x)
  setInverse <- function(solveMatrix)inv <<- solveMatrix
  getInverse <- function()inv
  list(set = set,get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calcs the mean of the vector created in the previous function
## It checks if it has previously been calculated and gets the mean if this is the case
## If not previously calculated, it calcs the mean and sets the value of the mean

cachesolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


