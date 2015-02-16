# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.
#
# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the cacheSolve should 
# retrieve the inverse from the cache.
#
# Example:
# > A = matrix(c(2, 1, 0, 0, 3, 0, 2, 2, 1, 3, -3, 3, 5, 1, 2, 1), nrow=4, ncol=4)
# > m = makeCacheMatrix(A)
# > cacheSolve(m)
# [,1] [,2] [,3] [,4]
# [1,]   18  -35  -28    1
# [2,]    9  -18  -14    1
# [3,]   -2    4    3    0
# [4,]  -12   24   19   -1
# > cacheSolve(m)
# getting cached data
# [,1] [,2] [,3] [,4]
# [1,]   18  -35  -28    1
# [2,]    9  -18  -14    1
# [3,]   -2    4    3    0
# [4,]  -12   24   19   -1
# > B = cacheSolve(m)
# getting cached data
# > B %*% A
# [,1] [,2] [,3] [,4]
# [1,]    1    0    0    0
# [2,]    0    1    0    0
# [3,]    0    0    1    0
# [4,]    0    0    0    1


# Return a special "matrix" object that can cache its inverse, which is really 
# a list containing a function to:
# 1. set the value of the matrix: set
# 2. get the value of the matrix: get
# 3. set the value of the inverse: setinverse
# 4. get the value of the inverse: getinverse

makeCacheMatrix <- function(x = matrix()) {
    # Return a special "matrix" object that can cache its inverse.
    inv <- NULL
    # Define the four functions:
    # 1. Function to set the value of the matrix.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # 2. Function to get the value of the matrix.
    get <- function() x
    # 3. Function to set the value of the inverse.
    setinverse <- function(inverse) inv <<- inverse
    # 4. Function to get the value of the inverse.
    getinverse <- function() inv
    # Return the list containing the four functions.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Compute the inverse of the special "matrix" returned by makeCacheMatrix 
# above, returning the cache if it has already been calculated.

cacheSolve <- function(x, ...) {
    # Compute the inverse of the special "matrix" returned by makeCacheMatrix.
    inv <- x$getinverse()
    # Check whether the inverse has already been calculated.
    if (!is.null(inv)) {
        # Inverse has already been calculated.
        message("getting cached data")
        # Return the cache.
        return(inv)
    }
    # Get the value of the matrix.
    data <- x$get()
    # Compute inverse using solve().
    inverse <- solve(data, ...)
    # Cache the inverse.
    x$setinverse(inverse)
    # Return the inverse.
    inverse
}
