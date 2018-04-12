#Assignment cache matrix inverse calculation

#Matrix inverse is usually a costly computation and there may be some benefit to caching
#the inverse of a matrix rather than compute it repeatedly.  
#The following function calculates inverse matrix created with the makeCacheMatrix function. 
#However, it first checks wether it has already been calculated and present in cache. 
#If so, it gets it from the cache and skips the computation.
#Otherwise, it calculates the inverse via the setinverse function.


makeCacheMatrix<- function(x = matrix()){
  im <- NULL 
  set <- function(y) {
    x <<- y
    im <<- NULL 
  }
  get <- function() x
  setinverse <- function(solve) im <<- solve
  getinverse <- function() im
  list(set = set, get = get,  
       setinverse = setinverse,  
       getinverse = getinverse)  
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

#Computing the inverse of a square matrix can be done with the solve function in R. 
#For example, if X is a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}

