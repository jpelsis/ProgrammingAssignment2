## Matrix inversion is usually a costly computation. The functions below
## create a matrix object caching the inverse of a matrix rather than
## compute it repeatedly. The first function creates a CaheMatrix object
## and the second function computes and caches the inverse of a CacheMatrix
## object if it has not already been cached.

## FUNCTION makeCacheMatrix
## INPUT    matrix object
## RETURN   list of CacheMatrix methods
## DESCRIPTION
## The function, makeCacheMatrix creates a special "matrix" object
## which contains methods for the following functions:
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse matrix
##  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) { # sets the value of the matrix object 'x'
    x <<- y 
    i <<- NULL
  }
  get <- function() x #returns the matrix object 'x'
  setinverse <- function(inverse) i <<- inverse #stores the inverse matrix of the matrix object 'x'
  getinverse <- function() i #returns the inverse matrix of the matrix object 'x'
  list(set = set, get = get, #returns a list of the methods of the 'x'
       setinverse = setinverse,
       getinverse = getinverse)
}


## FUNCTION cacheSolve
## INPUT    CacheMatrix object
## RETURN   inverse of CacheMatrix
## DESCRIPTION
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) { #check if there is already a chached inverse
    message("getting cached data")
    return(i) #return the already cached matrix
  }
  data <- x$get()
  i <- solve(data, ...) #solve for the inverse of x
  x$setinverse(i) #cache the inverse matrix in the object x
  i
}
