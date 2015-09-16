## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following functions cache the inverse matrix by using the "<<-"
## operator, that allows you to set the value of a variable in the parent environment


## The "makeCacheMatrix" function takes a matrix (x) as input and 
## creates a list of functions to store that matrix and cache its 
## inverse (m)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInvMtx <- function(InvMtx) m <<- InvMtx
    getInvMtx <- function() m
    list(set = set, get = get,
         setInvMtx = setInvMtx,
         getInvMtx = getInvMtx)

}


## The "cacheSolve" function takes in input the list created with
## the function "makeCacheMatrix".It first checks if the inverse matrix
## has already been calculated. If so, it gets the inverse matrix from the cache 
## and skips the computation. Otherwise, it calculates the inverse matrix 
## and stores it in the cache via the "setInvMtx" function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInvMtx()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInvMtx(m)
    m
}
