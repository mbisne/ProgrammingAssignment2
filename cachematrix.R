## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inversematrix <- NULL
    set <- function(x) {
        matrix <<- x;
        inversematrix <<- NULL;
    }
    get <- function() return(matrix);
    setinverse <- function(inv) inversematrix <<- inv;
    getinverse <- function() return(inversematrix);
    return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
    inverse <- mtx$getinv()
    if(!is.null(inverse)) {
        return(inverse)
    }
    data <- mtx$get()
    invserse <- solve(data, ...)
    mtx$setinv(inverse)
    return(inverse)
}
