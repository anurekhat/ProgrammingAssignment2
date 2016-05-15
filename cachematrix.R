## The below two functions when used in conjunction will calculate & cache the
## inverse matrix for a provided square (invertible) matrix.
## Sample Usage: 
##		cachedMatrix <- makeCacheMatrix(mx1)
##		inv1 <- cacheSolve(cachedMatrix) # calculates the inverse for mx1
##		...
##		rm(inv1)
##		...
##		inv1 <- cacheSolve(cachedMatrix) # returns the cached inverse
##		cachedMatrix$set(mx2)
##		inv2 <- cacheSolve(cachedMatrix) # calculates the inverse for mx2				


#	This function takes in a matrix and defines getters and setters for the 
#	matrix and its inverse. Returns a list containing the function definitions 
#	for the getters and setters

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
			x <<- y
			inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) inverse <<- inv
	getInverse <- function() inverse
	list (get= get, set = set, getInverse = getInverse, setInverse = setInverse)
}


#	This function takes in the custom list that's returned by the  
#	'makeCacheMatrix' and returns the calculated or previously cached inverse 
#	matrix as appropriate. It also takes in additional parameters that can be 
#	passed on to the 'solve' function 

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
		if (!is.null(inv)) {
			message("Returning cached data")
			return(inv)
		}
		# calculate the inverse & cache it.
		inv <- solve(x$get(),...)
		x$setInverse(inv)
		inv
}