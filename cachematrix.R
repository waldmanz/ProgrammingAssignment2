## Pair of functions - first defines a CacheMatrix "object" that stores
## a matrix and its inverse (if previously calculated),
## as well as basic functions to get/set the matrix
## and get/set the matrix inverse.  The defines a function that
## returns/caches the inverse of a CacheMatrix objection. 

## Define CacheMatrix object as 4 element list, each of which
## are functions (methods).  Each CacheMatrix object, when created,
## also contains its own value of itself (x) and its inverse (minverse)

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        get <- function()
                return(x)
        
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        getinv <- function()
                return(inverse)
        
        setinv <- function(inv)
                inverse <<- inv
        
        return(list(set=set, get=get,
                    setinv=setinv, getinv=getinv))
}


## Takes CacheMatrix object and returns its inverse
## Uses previously cached inverse if it exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        # get cached value of matrix inverse (may be NULL)
        inverse <- x$getinv()
        
        if (!is.null(inverse)){
                message('Using cached inverse')
                return(inverse)
        }

        else{
                # else solve for inverse and cache it in x
                inverse <- solve(x$get(), ...)
                x$setinv(inverse)
                return(inverse)              
        }
}
