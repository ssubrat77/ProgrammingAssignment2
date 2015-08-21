##  makeCacheMatrix: This function creates a special "matrix" object 
##  that can cache its inverse.
##
##  Sample Usage:
##		r <- rnorm(9)
##		m1 <- matrix(r, nrow=3, ncol=3)
##		tmp = makeCacheMatrix(m1)
##		cacheSolve(tmp)


makeCacheMatrix <- function(x = matrix()) {
    # -----------------------------------------------------------
    # Input x is an invertible matrix
    # This function returns a list containing following functions
    # 	1. set the value of matrix
    #   2. get the value of matrix
    #   3. set the value of inverse
    #   4. get the value of inverse	
    # -----------------------------------------------------------
    minv <- NULL
    set <- function(y) {
      # assign value (<<-) - cached
      x <<- y
      minv <<- NULL
    }
    
    get <- function() x
    setmat <- function(inverse) {
      minv <<- inverse 
    }
    getmat <- function() minv
    
    #Return the values as list
    list(set=set, get=get, setmat=setmat, getmat=getmat)
}


##  cacheSolve: This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. If the inverse has already been 
##  calculated (and the matrix has not changed), then the cachesolve should 
##  retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    # -----------------------------------------------------------
    # Input x is output of makeCacheMatrix() function
    # This function returns inverse of the input matrix
    # -----------------------------------------------------------
    minv <- x$getmat()
    
    # if the inverse has already cached in memory
    if (!is.null(minv)){
      # get the value from the cached memory 
      message("From cached data, skipping re-computation")
      return(minv)
    }
    
    # If not available in cache then compute the inverse 
    m1 <- x$get()
    minv <- solve(m1, ...)
    
    # Cache the computed value using setmat function for next run
    x$setmat(minv)
    return(minv)	
}
