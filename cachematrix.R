## These functions store the inverse matrix in cache and then return it
## makeCacheMatrix creates a list with a function to get and set the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## cacheSolve seeks the inverse matrix in cache and returns it if the function finds it
## If it's not in cache then it calculates the inverse matrix 
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached matrix inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

