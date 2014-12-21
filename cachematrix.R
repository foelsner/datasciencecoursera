## These two functions work together to cache the inverse of a matrix, and then to return this
## cached value should the inverse of the matrix be required again without re-calculating,



## makeCacheMatrix function description:

## This function (makeCacheMatrix) takes a matrix as its argument. It then creates a null variable 
## "i" in which to store the inverse of the input matrix. 

## "set" is available for re-settting ## the values in the cached matrix while also
## resetting the inverse variable so that cacheSolve will know to resolve this matrix when called again.

## "get" stores the input matrix.

## "setinv" creates a function for cacheVector to store the inverse matrix

## "getinv" creates a function to store the inverse matrix, which is initially NULL

## A list is returned with the previous four functions as its arguments.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve function description:

## "i" is set as the inverse matrix stored by the first function. If "i" is not NULL, then the cached
## value for the inverse matrix will be reported.

## If there is no cached inverse matrix, then this function will use the get function from makeCacheMatrix
## to read the original matrix data, and then use the solve() function to calculate its inverse. This inverse
## is then stored by setinv().

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}