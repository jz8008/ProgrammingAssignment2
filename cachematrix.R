
## The following two functions will get the cached inverse Matrix for a input 
## Matrix; if the cache is not available, it will calculate the inverse and 
## cache it


##This function creates a special "matrix" object
##that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  invx <- NULL
  set <- function(y){
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  
  setinverse <- function(solve){  
    invx <<- solve
  }
  
  getinverse <- function() invx
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  

}


##This function get the cached inverse Matrix out or if not being able to do so
##calculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx <- x$getinverse()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  
  data <- x$get()
  
  invx <- solve(data, ...)
  
  x$setinverse(invx)
  invx
  
  
}
