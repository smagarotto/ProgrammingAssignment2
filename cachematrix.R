## This function caches the inverse of a matrix so
## it is not necessary to computer it every time,
## but just to read the cache value.

## This function creates a special matrix 
## so I can cache the inverse.

makeCacheMatrix <- function(x = matrix()) 
{
	inv <- NULL
	set <- function(y)
	{
		x <<- y
		inv <<- NULL
	}
	
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## This function calculates the inverse of the special matrix
## and, if the inverse has already been calculated, it will
## show the inverse from the cache, if not calculate it.

cacheSolve <- function(x, ...) 
{
	## Return a matrix that is the inverse of 'x'
	
	inv <- x$getInverse()
	if (!is.null(inv))
	{
		message("getting cached data")
		return(inv)
	}
	matrice <- x$get()
	inv <- solve(matrice, ...)
	x$setInverse(inv)
	inv
}
