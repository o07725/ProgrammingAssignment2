## Programming Assignment 2: Lexical Scoping
## October 25, 2015
##
## The assignment consists of two functions: makeCacheMatrix, and cacheSolve.
## Inverting a matrix is time consuming.  These two functions cache inverted
## matrices to avoid duplicated calculations, hence; reducing the time required
## for calculations.
##
## The matrix must be square and invertible
##
## The flow of makeCacheMatrix, and cacheSolve follows the example "makeVector",
## and "cachemean" included in the Assignment Two instructions.  

## The function, makeCacheMatrix, creates an object that can cache the object's
## inverse.  

makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        set <- function (y) {
                x <<- y
                invr <<- NULL
        }
        ## The <<- is used to assign values outside the local environement.
        get <- function () x
        setinvr <- function(inverse) invr <<- inverse
        getinvr <- function() invr
        list (set = set, get = get, setinvr = setinvr, getinvr = getinvr)
        ## The above list is used as input to the cacheSolve function
}

## The list, from the makeCacheMatrix function serves as an input to the cacheSolve
## function.  The cacheSolve function tests to see if the inverse has been
## calculated (if (!is.null(invr))), if not, the inverse is calulated.
## solve is the generic function that inverts the matrix 
## (see ?solve for specifics)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invr <- x$getinvr()
        if (!is.null(invr)){
                message ("getting cached data")
                return(invr)
        }
        ## The above step notifies the user that the inverse is cached
        ## and does not need to be calculated; otherwise, the following
        ## is calculated.
        matrx.data <- x$get()
        invr <- solve(matrx.data, ...)
        x$setinvr(invr)
        return(invr)
}

## Note to fellow students: In addition to the Horne (2014) explanation,
## I found Wickham (2014) to be very helpful.  You can find Wickham online;
## however, I decided to purchase a printed copy. 
##
## References
##
## Horne, G. D. (2014). Understanding lexical scoping in R - Great guidance
## by community TA in Coursera.  Retrieved October 21, 2015 from
## https://asitarrives.wordpress.com/2014/10/18/understanding-lexical-scoping-in-r-great-guidance-for-community-ta-in-coursera/
##
## Wickham, H. (2014). Advanced R. Boca Raton, FL: CRC Press
