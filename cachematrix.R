## This is a set of two functions which which cache inverse of an invertible matrix
## This way inverse can be retrieved from cahce, instead of recomputing if the matrix repeats


## Function creates a special "matrix" object that can cache its inverse
## It returns list of functions, containing get and set methods for matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                #assumed that set will reset cache
                #hence x IDENTICAL y check performed before calling function
                x <<- y
                m <<- NULL
                message("Matrix set, cache cleared")
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns inverse of an invertible matrix
## Function first checks to see if inverse is cached
## If yes then inverse is returned from cahce
## Else compute inverse and cahce the inverse output

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("Inverse Retireved from Cache")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        message("Inverse Successfully Cached")
        m
}
