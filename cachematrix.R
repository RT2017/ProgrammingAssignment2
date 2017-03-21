## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix, can generate a matrix and cache inverse result if it has been calculated already 
## 
makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <<- NULL
	
	set <- function(y) {
		if (!isTRUE(all.equal(x ,y))) {
 			x <<- y
        	        inv_matrix <<- NULL
		}
	 }
        get <- function() x
        setInverse <- function(inverse) inv_matrix <<- inverse
        getInverse <- function() inv_matrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}





## Write a short comment describing this function
## cacheSolve can calcuate inverse of matrix x
## Return from cache if the inverse has been calculated                 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_m <- x$getInverse()
        if (!is.null(inv_m)) {
                message("return cached result")
                return(inv_m)
        }
        m <- x$get()
        inv_m <- solve(m)
        x$setInverse(inv_m)
        inv_m
}
