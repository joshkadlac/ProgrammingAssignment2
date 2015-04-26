## Create a module that can cache the inverse of a matrix 


## First function: Create an opject that you can pass along to 
## the cachesolve function

makeCacheMatrix <- function(x = matrix()) {
        ##  We need to do 5 things
        ## 1. Create an object and set the value to null
        ## 2. Set the value of the vector
        ## 3. Get the value of the vector
        ## 4. Set the inverse of the mean
        ## 5. Get the inverse of the mean
        
        ## 1. Create an object and set the value to null
        
        inverseVector <- NULL
        
        ## 2. Set the value of the vector
        setVector <- function(y) {
                x <<- y
                inverseVector <<- NULL
        }
        
        ## 3.  Get the value of the vector
        ## This is a function stored in an object
        
        getVector <- function() x 
        
        ## 4. Set the inverse of the vector
        ## This is creating a seperate object that
        ## holds the inverse of the vector
                
        setInverse <- function(solve) inverseVector <<- solve
        
        ## 5. Get the inverse of the mean
        ##  
        
        getInverse <- function()inverseVector
        
        ## List all of the functions defined so far
        list(setVector = setVector, getVector = getVector, 
             setInverse = setInverse, getInverse = getInverse)

}


## This function will calculate the inverse of the vector
##  created with th makeCacheMatrix function.  If the vector created 
## has already been calculated, and has not changed then the cached value 
## will be returned.

cacheSolve <- function(x, ...) {
        
        ## Import the matrix and load it into the object inverseVector
        
        inverseVector <- x$getInverse()
                
        ## Check to see if the cache already contains data and return it                
        if (!is.null(inverseVector)){
                message("Getting your cached data")
                return(inverseVector)
}

        ## If the above if statement failed, then get the data here
        data <- x$getVector()

        ## Get the matrix 
        inverseVector <- solve(data, ...)

        ## Cache the inversed data to be used next time
        x$setInverse(inverseVector)
        
        ## Return the inverse
        inverseVector
}

