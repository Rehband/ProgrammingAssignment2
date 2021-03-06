## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly.
## These functions compute and cache the inverse of a square matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	 
	m <- NULL
    
	get <- function() x
	set <- function(y) {
        	x <<- y
        	m <<- NULL
        }    
	
	getInverse <- function() m
	setInverse <- function(inverse) m <<- inverse    
	
	list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the cached version of the inverse.
cacheSolve <- function(x, ...) {
        
	m <- x$getInverse()
        if (!is.null(m)) {
        	message("getting cached data")
		return(m)
    	}
	
	data <- x$get()
	m <- solve(data)
	x$setInverse(m)	
	
	## Return a matrix that is the inverse of 'x'
	m
}

## Test

## Create matrix
M <- cbind(c(1,2,3),c(1,4,6),c(2,-3,-5))
m <- makeCacheMatrix(M)
m$get()
##      [,1] [,2] [,3]
## [1,]    1    1    2
## [2,]    2    4   -3
## [3,]    3    6   -5

cacheSolve(m)
## No cache in the first run
##      [,1] [,2] [,3]
## [1,]    2  -17   11
## [2,]   -1   11   -7
## [3,]    0    3   -2

cacheSolve(m)
## getting cached data...
##      [,1] [,2] [,3]
## [1,]    2  -17   11
## [2,]   -1   11   -7
## [3,]    0    3   -2

mi <- cacheSolve(m)   
mi%*%M
##      [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1
