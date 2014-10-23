## The code below is made of two functions.  In the first function,
## a matrix is created, in the second function the inverse of the created
## matrix is computed and returned. 
## However, if the code has been run on the same matrix before, and then 
## is run again, the second function will discover that the inverse was
## already computed and stored as cached data in the first function.
## Thus, it will return the cached data (or cached inverse)


## This function creates a matrix by either setting a new matrix or 'getting'
## an existing matrix (via 'set' and 'get'). It also caches an inverse
## computed by the 'cacheSolve' function (via getinv)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the matrix created in 'makeCacheMatrix'
## but first it checks to see if the inverse was computed already by checking
## if it is stored in the first function (via inv <- x$getinv())

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}