## Two functions that cache the inverse of a matrix.

## Function makeCacheMatrix. 
## Creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    if(!is.matrix(x))
    {
        stop("Argument must be a matrix.")
    }
    
    i <- NULL # Inverse of x.
    
    set <- function(y = matrix()) 
    {
        if(!is.matrix(y))
        {
            stop("Argument must be a matrix.")
        }
        x <<- y
        i <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inv = matrix()) i <<- inv
    
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
} # makeCacheMatrix

## Function cacheSolve.
## Computes the inverse of the matrix object returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not
## changed, then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) 
{
    inv <- x$getinverse()
    
    if(!is.null(inv)) { 
        message("Retrieving cached inverse.")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    message("Retrieving computed inverse.")
    inv

} # cacheSolve

