## This is a set of 2 functions
##   1. for implementing a matrix that can cache
##      its inverse
##   2. for returning the inverse of the matrix from
##      the cache, if available. 

## Takes a matrix as input and creates an object that
## can store the inverse in cache. The function returns
## 4 functions in the list, which can be used to access
## the matrix or its inverse 
## myvar$get() : to get the value of the matrix
## myvar$set() : to set the value of the matrix(not used)
## myvar$getinv() : to get the inverse of the object
## myvar$setinv() : to set the inverse of the object

makeCacheMatrix <- function(cachedMatrix = matrix()) {
    ## Variables are initialized in the environment create
    ## by makeCaceMatrix. This avoids it being created in
    ## the global environment and ensures 1 set of variable
    ## for each new vector created
    
    cachedInvOfMat <- NULL
    
    ## method to set the value of matrix in global env
    set <- function(mat = matrix()){
        cachedInvOfMat <<- mat
    }
    
    ## method to return/display the matrix
    get <- function(){
        cachedMatrix
    }
    
    ## method to assgin inverse  matrix argument to cache
    setInverse <- function(matrixArg) {
        cachedInvOfMat <<- matrixArg
    }
    
    ## method to return/display the inverse cached
    getInverse <- function(){
        cachedInvOfMat
    }
    
    ## return the methods and make it available for calling
    ## from another environment
    retVal <- list(set = set, 
                   get = get, 
                   setInverse = setInverse, 
                   getInverse = getInverse)
    attr(retVal, "modifiedType") <- "cacheMatrix"
    retVal
}  


## Write a short comment describing this function

cacheSolve <- function(matrixArg, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Check object cache if inverse is available,
    ## otherwise calculate, set cache and return value

    objAttr <- attr(matrixArg, "modifiedType")

    if(is.null(objAttr) || objAttr != "cacheMatrix"){
        stop("Passed Argument is not a correct type")
    } 
    
    inverse <- matrixArg$getInverse()
    if(is.null(inverse)){
        matrix <- matrixArg$get()
        inverse <- solve(matrix)
        matrixArg$setInverse(inverse)
    } else {
        print("Getting Cached Data....")
    }
    
    inverse
}
