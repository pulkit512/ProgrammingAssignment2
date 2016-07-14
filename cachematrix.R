## Caching the Inverse of a Matrix
## These functions will basically accept square invertible matrix as argument
## and will cache its inverse and return the inverse as output.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## It can set, get, getinverse and setinverse the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- matrix()
	m <- NULL
	set <- function(y = matrix()) {
		x <<- y
		m <<-NULL
	}
	get <- function() x	
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve will return the inverse of the matrix
## it will return inverse of matrix from cache if it exists in the cache
## otherwise it will calculate the inverse of matrix and will return it
## This inverse is also stored in the cache so that next time it can be
## directly fetched from cache.

cacheSolve <- function(x, ...) {        
		 m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
		## Return a matrix that is the inverse of 'x'
        m
}
