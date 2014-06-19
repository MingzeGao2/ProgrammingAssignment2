## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix create four functions (set, get, setinv, getinv) and two variable(inv, x) inv is the inverse of maxtrix x
## set init the matrix that we want to find inverse
## get returns the matrix
## setinv is used when we already computed the inverse of that maxtrix and cache it to inv
## getinv returns the value of inverse
makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL   
		set <- function(y){
		    x <<- y
		    inv <<- NULL
		}
		get <- function() x
		setinv <- function(inverse) inv <<- inverse
		getinv <- function() inv
		list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
##first cacheSolve check whether the inverse of that matrix is already been cached,if not it calculate the inverse and cache it into inv, otherwise just return the cached inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(! is.null(inv)){
	     message("getting cached data")
	     inv
	}
	inv <- solve(x$get())
	x$setinv(inv)
	inv
}
