## cachematrix.R
## Matrix storage function. Holds original matrix and inverse of matrix, once computed.
makeCacheMatrix <- function(matrixStore = matrix()) {
	inv <- NULL
	# sets matrix and default NULL of inverse into into global variables (matrixStore and inv)
	set <- function(y) { 
		matrixStore <<- y 
		inv <<- NULL
	}
	get <- function() matrixStore  					# Return current stored matrix
	setInverse <- function(solve){ inv <<- solve }	# Store an inverse of matrix
	getInverse <- function()inv  					# Return inverse of matrix
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Solves an invertible matrix and caches results. Additional calls to the function 
## will return previously cached results. NOTE: matrixStore must be previously initialized
## before calling this function.
cacheSolve <- function(matrixStore, ...) {
	# Return a matrix that is the inverse of 'x' 
	inv <- matrixStore$getInverse()
	#if results are returned (not NULL), then function will outpu cached results
	if(!is.null(inv)) {
		message("getting inverse matrix data")
		#display stored results
		return(inv)
	}
	# No previous cached results, solve from passed in matrix storage function
	inv <- solve(matrixStore$get())
	# Store matrix inverse into matrix storage function
	matrixStore$setInverse(inv)
	# Display results
	inv
}
	
	
