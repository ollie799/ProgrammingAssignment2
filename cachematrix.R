
# creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set <- function(y) {
            x <<- y
            inv_matrix <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv_matrix <<- solve
        getsolve <- function() inv_matrix
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


# computes the inverse of the special "matrix" returned by makeCacheMatrix unless already done
# if so, then retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}



