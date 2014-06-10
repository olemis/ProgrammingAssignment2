## Cache potentially time-consuming computation

## makeVector creates a special "vector", which is really a list 
## containing functions to 
#
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
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


## Calculate the inverse matrix of the special "vector"
## created with the above function.
## However, it first checks to see if it has already
## been calculated. If so, it gets the value from the cache
## and skips the computation. Otherwise, it calculates
## the inverse matrix of the data and sets the value of the value
## in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
