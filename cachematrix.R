## These functions allow data to be read from a cache, rather
## than be recomputed

## makeCacheMatrix is used to create a special object that
## contains functions to set/get matrices and their inverses
makeCacheMatrix <- function(x = matrix()) {
    
    ## set i (the inverse) to NULL
    i <- NULL
    
    ## set function will: set x (old matrix) to argument y 
    ## (new matrix) and i (the inverse) to NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## get function will: return x (the matrix)
    get <- function() { x }
    
    ## setinverse function will: set i to inverse
    setinverse <- function(inverse) { i <<- inverse }
    
    ## getinverse function will: return i (the inverse)
    getinverse <- function()  { i }
    
    ## returns a vector of all functions
    list(set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve will check to see if the matrix inverse has already been
## calculated (if so it will read the result from the cache), or will
## solve it if not already calculated. 
cacheSolve <- function(x, ...) {
    
    ## attempts to get inverse if it was already calculated
    i <- x$getinverse()
    
    ## if inverse was already calculated: i will be returned with
    ## a message saying "getting cached data"
    if(!is.null(i)) {
            message("getting cached data")
            return(i)
    }
    
    ## if inverse was not already calculated: set data to x 
    ## (the matrix), and calculate the inverse and use the
    ## setinverse function on it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    
    ## return i (the inverse)
    i
}
