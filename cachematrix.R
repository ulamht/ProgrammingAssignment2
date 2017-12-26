## Functions that cache the inverse of a matrix

## The function is used to create a special object 
## that stores a numeric vector and cache's its matrix.

## Create a special "matrix", which is a list containing
## a function to
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse matrix
##   4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
       x <<- y
       i <<- NULL
     }
     get <- function() x
     setinverse <- function(inv) i <<- inv
     getinverse <- function() i
     list(
       set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
     )
}


## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        m <- x$get()
        i <- solve(m, ...)
        x$setinverse(i)
        i
 }
