## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix type with get and set methods for the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <-  function(y)
        {
                x<<-y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv<<- inverse
        getinverse <- function() inv
        list(set = set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## This function accepts the special matrix type from the above function, 
###checks for a cached inverse and returns it, otherwise computes the inverse of the matrix and returns it.

cacheSolve <- function(x, ...) {
        ##This gets the inverse of the matrix if it is stored.
        inv <-  x$getinverse()
        if(!is.null(inv))
        {
                #This code executes only if there is a stored inverse to the matrix.
                message("getting cached data")
                return(inv)
        }
        #This code executes if no inverse is cached, and it caches its computed inverse for the future.
        data1 <- x$get()
        inv <- solve(data1)
        x$setinverse(inv)
        return(x$getinverse())
}
