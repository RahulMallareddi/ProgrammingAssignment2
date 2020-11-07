##  Caching the Inverse of a Matrix
## This function cache its inverse for the input (which is an invertible square matrix).

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
}
get <- function() x
setinv <- function(inverse) inv <<- inverse
getinv <- function() inv
list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##This function inverse from the cache.

cacheSolve <- function(x, ...) {
       inv <- x$getinv()
if(!is.null(inv)) {
message("getting cached result")
return(inv)
}
data <- x$get()
inv <- solve(data, ...)
x$setinv(inv)
inv  
}
