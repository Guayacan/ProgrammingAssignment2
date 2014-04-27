## These two functions together save computing time by looking for a matrix  inverse in the cache before making the calculation. If the matrix exists in the cache, it prints out the inverse immediately, if not, it will calculate it. 

## this function creates a matrix that is stored, and that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#this function gets the inverse of a matrix if it was used before. If it was, the message "getting cached data" prints out, and the matrix is shown. If it was not in the cache, it will calculate the inverse, store it and print it.  

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}