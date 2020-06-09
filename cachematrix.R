## Matrix inversion can consume a lot of time is it needs to be done everytime
## in a loop. However, it comes very handy if we can cache the inverse of a 
## matrix. This method can save a lot of time. Following two functions are used 
## to perform exactly that - getting the caching the inverse of the matrix and 
## returning it.


## The makeCacheMatrix function takes a matrix in argument and assigns it to an 
## object in an environment different than current environment. It creates a list
## containing a function to set the value of a matrix, get that value, set its 
## inverse and get that inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setInvMat <- function (inverse) inv_mat <<- inverse
        getInvMat <- function () inv_mat
        list(set = set, get = get,
             setInvMat = setInvMat,
             getInvMat = getInvMat)
        
}




## This function takes an invertible matrix.
## It checks if there is already an inverse matrix computed in the cache.
## If yes, it skips the computation and gets the inverse matrix from the cache
## after printing the message "getting cached data".

cacheSolve <- function(x, ...) {
        inv_mat <- x$getInvMat()
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        
## If the inverse matrix is not there in the cache,
## it sets it in the cache for further use and returns the inverse matrix. 
        
        data <- x$get()
        inv_mat <- solve(data)
        x$setInvMat(inv_mat)
        inv_mat
}


## Testing the function on a sample square matrix

# x <- matrix(1:4, nrow=2, ncol=2)
# > m <- makeCacheMatrix(x)
# > m$get()
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4

## No cache in the first run
# > cacheSolve(m)
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
## The inverse matrix thus calculated is set in the cache now.

## Retrieving from the cache in the second run
# > cacheSolve(m)
# getting cached data
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
##
