## Overall These functions work together in order to store in memory the inverse of a Matrix.
## This is useful because inversing a matrix is time consuming and we don't want to do it again unless
## recalculation is required.

## MakeCache Matrix a Matrix Object (parent) containing 4 child functions (stored in a list as get,set,getinverse and set inverse). it also stores the Matrix Input
## Its essentially a holding area for cached matrixes (if CacheSolve has been run). 
## Before its friend Cache Solve has been run.this function just stored the List plus the origional matrix input
## After cacheSolve has been run - this object also stores the inverse (as setInverse - saved here in Line 39) alongside the origional Matrix. (get)
## This inverse will be pulled out each time by Cache Solve unless new inputs are entered into MakeCache Matrix - hence saving time

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function (y) {
        x <<-y
        inv <- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse 
    getInverse <- function() inv
    list(set = set, get = get,setInverse = setInverse, getInverse = getInverse)
     
    
    }


## The two bits of code that interact with makeCache Matrix are inv <- MatrixObject$getInv() & MatrixObject$setInverse(inv)
## These essentially retreive from memory previously calculated Inverse (if available) If not available because its th first time running the above function
## or if a new Matrix has been created (in which case inv will be set to NULL), then the calculation is preformed again and returned at the end of the function
## MatrixObject$setInverse(inv) resaved this inverse back to the origional function for future use.

cacheSolve <- function(MatrixObject, ...) {
    inv <- MatrixObject$getInv()
    if(!is.null(inv)){
        message("Getting Cache Data")
        return(inv)
        
    }
    MatrixToBeInverted <- MatrixObject$get()
    inv <- solve(MatrixToBeInverted,...)
    MatrixObject$setInverse(inv)
    inv
    
        ## Return a matrix that is the inverse of 'x'
}



