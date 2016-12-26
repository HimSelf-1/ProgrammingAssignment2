setwd("C:/Users/Dave/Desktop/Dad/Big_data/coursera_working_files/ProgAssignment3-data")
## rm(list=ls())  # clean the workspace  
## sample data: 
## x <- c(4,-4,-2,10)
## x<-matrix(x,nrow=2,ncol=2)

## x <- c(1,2,2,2)
## x<-matrix(x,nrow=2,ncol=2)
## x
## MakeMatrix <- makeCacheMatrix(x)
## cacheSolve(MakeMatrix)

## functions take an invertible square matrix and returns its inverse
## Below are two functions that are used to create a special object that 
## stores a numeric vector and caches its inverse.

## makeCacheMatrix: This function creates a special "matrix" object (which is really a list) that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  ## 1. set the value of the vector
        set <- function(y) {      ## defines a function to set the vector, x, to a new vector,  y, and resets m to NULL
                x <<- y
                m <<- NULL
        }
        get <- function() x   ## 2. get the value of the vector (returns the vector x)
        set_inverse <- function(solve) m <<- solve ## 3. set the value of the inverse of a matrix  sets m to the inverse of the matrix
        get_inverse <- function() m  ## 4. get the value of the inverse of a matrix  returns m
        
        list(set = set, get = get,    ## returns the 'special vector' containing all of the functions just defined
             set_inverse = set_inverse,
             get_inverse = get_inverse)
 }

## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)  ## calculates the inverse of the data and sets the value of the inverse 
                          ## in the cache via the set_inverse function
        m
}