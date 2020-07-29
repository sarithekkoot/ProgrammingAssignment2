makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y){
    x <<- y
    I <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {I <<- inverse}
  getInverse <- function() {I} 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  
  I <- x$getInverse()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  mat <- x$get()
  I <- solve(mat,...)
  x$setInverse(I)
  I
}
