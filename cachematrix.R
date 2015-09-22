## makeCacheMatrix creates creates a special "matrix" that 
## can cache its inverse
## makeCacheMatrix is really a list containing the following functions
## 1. set 			Sets the value of the matrix
## 2. get 			Returns the value of the matrix
## 3. setinverse 	Sets the value of the inverse of the matrix
## 4. getinverse 	Returns the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(xinv) inverse <<- xinv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the "special" matrix
## returned by makeCacheMatrix
## If the inverse of the matrix was already computed its value is
## retrieved from the cache

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse        
}
