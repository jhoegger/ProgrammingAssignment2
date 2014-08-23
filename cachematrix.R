## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {

    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(x) s <<- solve
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


##CacheSolve: 
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'

    s <- x$getinverse()               # accesses the object 'x' and gets the value of the inverse
    if(!is.null(s)) {              # if inverse was already cached (not NULL) ...
        
        
        
        message("getting cached data")  # ... send this message to the console
        return(s)                       # ... and return the inverse ... "return" ends 
        #   the function cachemean(), note
    }
    data <- x$get()        # we reach this code only if x$getinverse() returned NULL
    m <- solve(data, ...)   # if m was NULL then we have to calculate the inverse
    x$setinverse(s)           # store the calculated inverse value in x (see setinverse() in makeCacheMatrix)
    s                      # return the inverse to the code that called this function
        
    
}
