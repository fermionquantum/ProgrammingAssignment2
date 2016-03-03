## This functions creates a cached inverse of a matrix for do not 
## repeatedly this costly operation. For this, below there are two functions that
## created a special object that stores a matrix and is able to cache its inverse

## makeCacheMatrix() below creates a matrix that cached its inverse

makeCacheMatrix <- function(x = matrix()) 
{
    i <- NULL
    set <- function(y)
    {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Compute the inverse matrix by the function makeCacheMatrix(). 
## The command solve() realize this operation.
## This function first check to see if the inverse has already been calculated,
## if so, it gets the inverse from the cache and skip the calculation.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    if(!is.null(i))
    {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
