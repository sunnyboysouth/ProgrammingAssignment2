## Asignment 2: to understand Lexical Scoping we are writing a function
## to store the inverse of Matrix and retrive it from cache if called again

## The below function can cache the inverse of Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## the below function reads the Inverse from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  # Inverse is not cached so calculate the inverse  
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv  
}
