## Functions to cache a matrix object, 
## by creating a wrapper object over the matrix() object and adding functions 
## to do operations and save the inverted matrix inside the same object
## & later if the matrix is inverted before the result is retrieved without calculating inverted matrix again

## Takes as argument the matrix, and returns a list of functions to do on the passed matrix object.
## Encapsulates the inverted matrix of the original matrix.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() x
        setInvertedMatrix <- function(i) m <<- i
        getInvertedMatrix <- function() m
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInvertedMatrix = setInvertedMatrix,
             getInvertedMatrix = getInvertedMatrix)
        
}

## Takes as argument an object of type makeCacheMatrix, and calculates the inverted matrix of that matrix only once.
## But Returns it (the inverted matrix) as many times as called.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                
        m <- x$getInvertedMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$getMatrix()
        m <- solve(data,...)
        x$setInvertedMatrix(m)
        m
}

