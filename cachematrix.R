
## Chaching the inverse of a Matrix
## A cache is a way to store objects in memory to accelerate subsequent access to the same object.
## Chaching the inverse of a matrix is more benefitial than computing it repeatly.

## makeCaheMatrix : This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve : This function calculates the inverse of the special "matrix" created with the above function.
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.
## Otherwise, it calculates the inverse and sets the value in the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  } 
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## a sample matrix

m <- matrix(1:4, nrow = 2, ncol = 2)
makeCacheMatrix(m)
myMatrix_object<- makeCacheMatrix(m)
myMatrix_object$get()

## > myMatrix_object$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

cacheSolve(myMatrix_object)

## > cacheSolve(myMatrix_object)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## > cacheSolve(myMatrix_object)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5