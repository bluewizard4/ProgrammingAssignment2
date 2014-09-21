## The following functions can be used to make and cache the inverse 
## of a matrix.  If the inverse of the matrix has already been calculated
## then the functions can be used to pull the cached inverse without 
## recalculating the inverse of the matrix. 

## The following code structures and logic were influenced heavily by 
## the sample code provided by Roger D. Peng at 
## https://github.com/rdpeng/ProgrammingAssignment2

## makeCacheMatrix is a function that creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is a function that computes the inverse of the special "matrix"
## returned by the "makeCacheMatrix" function.  If the inverse has already 
## been calculated and the matrix is the same, then "cacheSolve" retrieves 
## the inverse of the matrix from the cache. 

cacheSolve <- function(x) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix2 <- x$get()
        m <- solve(matrix2)
        x$setinverse(m)
        m
}
