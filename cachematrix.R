## Two functions where the first makes a cached matrix and the second calculates 
## the inverse of the matrix using the cached version if available

## Makes a matrix x

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the inverse of the matrix x using cached matrix if available

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)){
           message("getting cached data")
           return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setinverse(m)
       m
}
