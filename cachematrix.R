## functions take an invertible square matrix and returns its inverse

## 'makeCacheMatrix' function takes x (a Vector or List) and creates a square matrix; byrow is FALSE  NOTE: per assignment, matrix supplied is always an invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- matrix(x, nrow = sqrt(length(x)), ncol = sqrt(length(x)))
        ##print(m)
        set <- function(y) {
                        x<<- y
                        m<<- m
        }
       ## 
        get<- function() x
        ## get <- function() x
       ## print(get)
        set_inverse <- function(solve) m <<- inverse
        get_inverse <- cacheSolve(m)
        list(set = set, get = get,
            set_inverse = set_inverse,
            get_inverse = get_inverse)
        
        ##return(m)
        ## sample data: 
                ##   makeCacheMatrix(c(4,-4,-2,10))
                ##   makeCacheMatrix(c(1,2,2,2))
        ## m
        ##             [,1] [,2]
        ##        [1,]    4   -2
        ##        [2,]   -4   10        
        }

## the function 'cacheSolve' inverts the invertible square matrix

cacheSolve <- function(x, ...) {
        m <- x
        ##print(m)
        if(!is.null(m)) {                       ## if inverse has already been calcuated, pull inverse from cache memory, else calculate inverse
                message("getting cached data")
        }
        data <-m
        m <- solve(data, ...) 
        ## print(m)
        m
        ## Return a matrix that is the inverse of 'x'
                ##           [,1]   [,2]
                ##        [1,] 0.3125 0.0625
                ##        [2,] 0.1250 0.1250
        
}
