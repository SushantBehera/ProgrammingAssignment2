## makeCacheMatrix function creates a special "matrix" object with help of 
## inline getter and setter functions that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # variable to store inverse of a matrix
    imat <- NULL
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        imat <<- NULL
    }
    
    # get the value of the matrix
    get <- function(){
        x  
    } 
    
    # set the value of the inverse
    setInverse <- function(inv){
        imat <<- inv  
    } 
    
    # get the value of the inverse
    getInverse <- function(){
        imat
    }
    
    # return list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    imat <- x$getInverse()
    
    # Check if inverse matrix is available in cache
    if (!is.null(imat)) {
        message("retriving data from cache")
        return(imat)
    }
    else{
        # get the matrix
        mat <- x$get()    
        
        # compute the inverse of input matrix
        imat <- solve(mat, ...)
        
        # set inverse of mat in cache
        x$setInverse(imat)
        
        # return
        imat
    }
}

