# Caching the Inverse of a Matrix:

## storing a matrix and caches its inverse.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}



## cacheSolve function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

## ---------------Checking the program------------------------
## m <- matrix(rnorm(16),4,4)
## m1 <- makeCacheMatrix(m)
## cacheSolve(m1)
##[,1]        [,2]        [,3]       [,4]
##[1,] -0.05563733 -0.68622860 -0.50293645 -0.2124612
##[2,] -0.22027870 -0.09643621  0.54165229  0.1403397
##[3,] -0.36378007  0.14032506 -0.20304105  0.1512100
##[4,] -0.21345237  0.17913817  0.03380426  0.6374888
