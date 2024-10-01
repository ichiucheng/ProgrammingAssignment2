## a pair of functions creating a special matrix object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {
  
## initialize the inverse property
  
  i <- NULL
  
## set the matrix

    set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
## get the matrix
    
  get <- function() {
    
## return the matrix
    
    m
  }
  
## set the inverse of the matrix
  
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
## get the inverse of the matrix
  
  getInverse <- function() {
    
## return the inverse property
    
    i
  }
  
## return a list of the methods
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## 1. compute the inverse of the special matrix returned by "makeCacheMatrix"
## 2. if the inverse has been calculated (matrix unchanged), then "cachesolve" retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  
## return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  
## just return the inverse if its already set
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
## get the matrix from our object
  
  data <- x$get()
  
## calculate the inverse using matrix multiplication
  
  m <- solve(data) %*% data
  
## set the inverse to the object
  
  x$setInverse(m)
  
## return the matrix
  
  m
}
