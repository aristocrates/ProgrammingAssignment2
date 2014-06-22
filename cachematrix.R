## The two functions in this R source file, makeCacheMatrix and cacheSolve,
## allow the user to cache the result of the inverse of a matrix, which is
## a potentially time-consuming operation.
## This is accomplished by creating a special list which is in essence a
## wrapper around a provided matrix which gives methods to update fields to
## cache the results of the inverse operation.
## Precondition: matrices used must be invertible
## Example use:
## m <- makeCacheMatrix(<put matrix you want to use here>)
## cacheSolve(m)

## Creates a special wrapper around the provided matrix
## to allow for caching the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse = NULL
        ## sets data to hold new matrix, clears cache of the inverse
        set <- function(y) {
                x <<- y
                cachedInverse <<- NULL
        }
        ## gets the data (aka the matrix)
        get <- function(){##added brackets to make the intent clearer
                x
        }
        ## caches the value inverse passed to the function
        setInverse <- function(inverse){
                cachedInverse <<- inverse
        }
        ## gets the cached value of the inverse. Returns null
        ## if the inverse has not yet been cached
        getInverse <- function(){
                cachedInverse
        }
        ## return a list of functions for the wrapper object
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Solves for the inverse of the "special matrix" x,
## where x is a matrix "wrapped" by the function makeCacheMatrix.
## If cached data is being used, sends the message "getting cached data"
## using the message function.
## Precondition: x is an object created with no errors by the method
##               makeCacheMatrix using an invertible matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if (!is.null(inverse)){ ## if the inverse is cached
                ## get the cached data
                message("getting cached data")
                return(inverse)
        }
        ## if the inverse is not cached
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        ## return the inverse that was just calculated
        inverse
}
