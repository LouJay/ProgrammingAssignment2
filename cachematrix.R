## These functions in conjunction allow for an efficient use of memory and
## resources by only performing the calculation of the inverse of a matrix if 
## it hasn't been calculated and stored previously in memory.

## makeCacheMatrix returns a special matrix object which can cache its inverse.
## In reality this special matrix object is a list with four functions, which
## are the ones used to either set or get the two objects stored within it: the 
## matrix "x" and the inverse "inv".  The "<<-" operator is crucial in this 
## case, since the normal "<-" operator would only store the "x" and "inv" 
## objects inside the functions that manipulate them, instead of storing them at 
## the parent level (the speciallist itself).  "<<-" makes the "x" and "inv" 
## objects available to the four functions of the list (without them being in
## the global environment).
## Strictly speaking, the "inv" object could be anything related to the matrix
## "x" since "makeCacheMatrix" doesn't actually calculate the inverse, it just
## has a function to get either of the objects store within it ("x" or "inv"), 
## a function to store "x" which also "empties" the value of "inv", and a
## function to store "inv" (which could be anything, but in this case we are
## storing the output of the function cacheSolve, which returns the inverse of 
## "x").

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve is to be used in conjunction with the specific lists created by 
## makeCacheMatrix, since it uses three of the four functions contained in those
## lists (which we called "special matrix object" previously).  cacheSolve first
## checks if the inverse of the matrix is already stored within the special list
## and returns it if that's the case, if not, it calculates the inverse, stores
## it within the special list, and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

## Example input and output (notice the "getting cached data" message that
## appears the second time that cacheSolve is executed without changing the 
## matrix stored within "specmat"):
## > specmat<-makeCacheMatrix(matrix(1:4,2,2))
## > specmat$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(specmat)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(specmat)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > specmat$set(matrix(c(2,6,10,11,3,4,9,8,23),3,3))
## > specmat$get()
##      [,1] [,2] [,3]
## [1,]    2   11    9
## [2,]    6    3    8
## [3,]   10    4   23
## > cacheSolve(specmat)
##              [,1]        [,2]        [,3]
## [1,] -0.059870550  0.35113269 -0.09870550
## [2,]  0.093851133  0.07119741 -0.06148867
## [3,]  0.009708738 -0.16504854  0.09708738
## > cacheSolve(specmat)
## getting cached data
##              [,1]        [,2]        [,3]
## [1,] -0.059870550  0.35113269 -0.09870550
## [2,]  0.093851133  0.07119741 -0.06148867
## [3,]  0.009708738 -0.16504854  0.09708738

