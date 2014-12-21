## This package defines a constructor and accessing functions to store and access a matrix
## with it cached matrix inverse.
## The package uses R's scoping mechanism to store the private state of the object and define
## accessing functions.
## 
## makeCacheMatrix is the object constructor
## makeSolve calculates, caches, and returns the matrix's inverse
## The object's other accessing functions are elements of the object's value -- a named list

## makeCacheMatric is the object constructor
makeCacheMatrix <- function(x = matrix()) {
    ## x is initial matrix value
    ## returns a named list of the functions to acccess the matrix
    ##   set() to update the matrix's value
    ##   get() to return the matrix's current value
    ##   setInv() to set the cached inverse
    ##   getInv() to return the cached inverse (NULL if not calculated)
 
    #create cached inverse in functions' private environment
    inv <- NULL     
    
    ## Create each function that acts like a method for correctly accessing x and its cache
    
    ## set() <- value   Sets the value of matrix to the provided value, and clears cache
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## get() Returns the value of the matrix
    get <- function() x
    
    ##setInv() Sets the cached matrix inverse
    setInv <- function(inverse) inv <<- inverse
    
    ##getInv()  Returns cached matrix inverse, or NULL if not in the cache
    getInv <- function() inv
    
    ## returns a list of the accessing functions
    list(set=set, get=get, setInv=setInv, getInv=getInv) 
}


## computes the inverse of object's matrix, stores it in the cache, and returns the inverse value
cacheSolve <- function(x, ...) {
    ## x is the augmented matrix object
    ## ... are additional optional passed through to solve(x)
    ## Returns a matrix that is the inverse of the stored matrix
    
    inverse <-x$getInv()        # retrieve the cached inverse from the environment
    if(!is.null(inverse)) {     # cached inverse is valid and can be used
        message("getting inverse from cache")
        return(inverse)
    }
    # cached inverse was not valid, calculate and return
    data <- x$get()
    inverse <- solve(data)
    x$setInv(inverse)
    inverse
}
