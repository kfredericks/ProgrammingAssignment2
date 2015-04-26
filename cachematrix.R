## The below functions create an inversible matrix-like object before
## using the Solve() function to compute the inverse of the matrix and
## cacheing the solution.

## makeCacheMatrix creates a special inversible matrix-like object, 'x',
## that can then cache its inverse, inverse of 'x'. 

makeCacheMatrix <- function(x = matrix()) {
			inv <- NULL
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            setinv <- function(inv) inv <<- inverse
            getinv <- function() inv
            list(set = set, 
            	get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## cacheSolve computes the inverse of the matrix-like object 'x' that 
## was returned by makeCacheMatrix above. If 'x' remains unchanged and 
## the inverse of 'x' has already been calculated, cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        
        	inv <- x$getinv()
        		if(!is.null(inv)) {
        				message("getting cached data")
        				return(inv)
 			}
 			data <- x$get()
 			inv <- solve(data, ...)
 			x$setinv(inv)
 			inv
}
