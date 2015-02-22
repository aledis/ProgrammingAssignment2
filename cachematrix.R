## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a closure over the matrix passed in 
## as a parameter with methods to access that matrix and 
## to store/retrieve inverse of that matrix

makeCacheMatrix <- function(matrix = matrix() ) {
        inverse <- NULL
        
        ## closure over matrix
        getMatrix <- function() matrix
        
        ## cache inverse
        setInverse <- function(inverseMatrix) inverse <<- inverseMatrix 
        ## get cached inverse
        getInverse <- function() inverse
        
        list(getMatrix = getMatrix, setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve retrieves the inverse of the matrix from cache 
## if it is available, otherwise inverse matrix is calculated
## and stored in cache

cacheSolve <- function(x, ...) {
        ## get cached inverse
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)) {
                ## cached inverse is not null, so return it
                return(inverseMatrix)
        }
        ## cached inverse was null, get matrix itself and
        ## calculate the inverseinverse
        matrix <- x$getMatrix()
        inverseMatrix <- solve(matrix)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
