## Put comments here that give an overall description of what your
## functions do

## This function reads the matrix and sets the setter and getter functions
## for both the original matrix (M) and the inverse matrix (MI). It does not 
## work by itself but in conjuction with the following function.

makeCacheMatrix <- function(M = matrix()) {
    MI <- NULL
    set <- function(y) {
        M <<- y
        MI <<- NULL
    }
    get <- function() M
    setinv <- function(solve) MI <<- solve
    getinv <- function() MI
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function reads the original matrix (M), checks whether the inverted 
## matrix (MI) remains as a null object or if a value has been assigned to it.
## Reads the matrix data and uses the solve function to calculate and return 
## the inverse matrix.

cacheSolve <- function(M, ...) {
    ## Return a matrix that is the inverse of 'M'
    MI <- M$getinv()
    if(!is.null(MI)) {
        message("getting cached data")
        return(MI)
    }
    data <- M$get()
    MI <- solve(data, ...)
    M$setinv(MI)
    MI
}


## Example

a1 <- c(3, 2, 5)
a2 <- c(2, 3, 2) 
a3 <- c(5, 2, 4)
A <- rbind(a1, a2, a3)

cacheSolve(makeCacheMatrix(A))

