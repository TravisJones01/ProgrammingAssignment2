## Put comments here that give an overall description of what your
## functions do
## First I had to learn what an inverse matrix does - see https://www.mathsisfun.com/algebra/matrix-inverse.html if you are interested
##First function takes, gets, and sets a matrix and inverse matrix.
##The second checks the first for NULL; if NULL calculates by pulling matrix from first function; 
##if not null returns inverse from first function

## makeCacheMatrix takes, gets, and sets a matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        InvMatrix <- NULL       
        SetMatrix <- function(y) {
                x <<- y
                InvMatrix <<- NULL
        }
        GetMatrix <- function() x
        SetInverse <- function(inverse) InvMatrix <<- inverse
        GetInverse <- function() InvMatrix      
        list(SetMatrix = SetMatrix, GetMatrix = GetMatrix, 
             SetInverse = SetInverse, GetInverse = GetInverse)
}


## cacheSolve checks the first for NULL; if NULL calculates by pulling matrix from first function; 
##if not null returns inverse from first function

cacheSolve <- function(x, ...) {
        InvMatrix <- x$GetInverse()
        if(!is.null(InvMatrix)) {
                message("Getting cached data")
                return(InvMatrix)
        }
        data <- x$GetMatrix()
        InvMatrix <- solve(data, ...)
        x$SetInverse(InvMatrix)
        InvMatrix
}

