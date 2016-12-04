## Object storing a matrix (assumed to be square) and its inverse. 
## Constructor allows the setting of the matrix at instantiation.
## Example: 
## x <- makeCacheMatrix()                   # Instantiate an object with no 
##                                          # defined matrix
## x <- makeCacheMatrix(matrix(1, 2, 2))    # Instantiate an object with 2x2 
##                                          # matrix containing 1
## Object has the following functions: 
## 1. Setting matrix value
## Example:
## x$set(matrix(0, 2, 2))   # Explicit setting of the matrix. Cached inverse is
##                          # set to NULL
##
##
## 2. Getting matrix value
## Example:
## x$get()                  # Returns the matrix
##
##
## 3. Setting matrix inverse value
## Example:
## x$setInv(myInv)          # Set the inverse as myInv
## 
## 
## 4. Getting matrix inverse value
## Example:
## x$getInv()               # Returns the stored inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setInv <- function(myInv) invMatrix <<- myInv
    getInv <- function() invMatrix
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}





## Function returns the inverse of the matrix in object x. It calculates and 
## stores the inverse if it has not been calculated and returns the cached 
## value if it was stored previously.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getInv()
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    invMatrix <- solve(x$get()) # Math function to get inverse
    x$setInv(invMatrix)
    invMatrix
}