## Put comments here that give an overall description of what your
## functions do:
## The function makeCacheMatrix() returns a matrix object 
## to store the original data matrix and its inverse.
## Required input is a matrix object.
## The function cacheSolve() returns the inverse of a matrix object
## created from makeCacheMatrix.
## If the inverse has been computed and stored in the cache, it returns
## the cached value, otherwise the inverse is computed and cached.


## Write a short comment describing this function:
## makeCacheMatrix creates a special "matrix", 
## i.e. a list containing functions 
## 1.set: set the value of the data matrix
## 2.get: get the value of the data matrix
## 3.setmatrix: set the value of the inverse matrix
## 4.getmatrix: get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(imatrix) m <<- imatrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Write a short comment describing this function:
## The function "cacheSolve" calculates the inverse of the special "matrix" created with "makeCacheMatrix". 
## It first checks to see if the inverse has already been calculated via getmatrix(). 
## If so, it uses and returns the inverse from the cache and skips the computation. 
## Otherwise, it obtains the data matrix by applying get() and calculates the inverse of the data matrix.
## Then it sets the value of the matrix in the cache via the setmatrix() function.


cacheSolve <- function(x, ...) {
	  m <- x$getmatrix()
	  ## check existence of inverse matrix
        if(!is.null(m)) {
                message("getting cached matrix data")
		    ## return chached inv matrix and exit function
                return(m)
        }
	  ## calculates, sets and returns inv matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
