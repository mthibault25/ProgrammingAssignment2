## Author: Mark Thibault
## Date: May 19, 2019
## Pupose: Will compute an inverse of a matrix and cache the result
##         When the inverse is retrived the cached version will be retrieved first
##         If no cached version exists, a new inverse will be generated

## Write a short comment describing this function
## This function will make an object that will be used to store the matrix
## set = set the matrix in the object
## get = retrieve the matrix
## setInverse = set the inverse of the matrix after being generated
## getInverse = retrieve the generated inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function
## Will retreive the cached generated inversed matrix if already cached
## If the matrix inverse is not cached then it will generate a new one
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message('Retriving cached data')
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
