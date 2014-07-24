## This function is designed to facilitate the calculation of the inverses of matrices. It contains functions that
## set and retrieve a matrix, calculate and cache the inverse of the matrix, and set and retrieve the inverse of
## the matrix. 

## This first function is actually a bundle of four functions. 
## Set will declare a new matrix y to replace the matrix x argument. Doing so will also reset the cached inverse 
##     i to a null value.
## Get returns the declared matrix x
## Seti declares the cached inverse i to be equal to the inverse of matrix w.
## Geti returns the cached inverse i.

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
	set<-function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	seti <- function(w) i<<-w
	geti <- function() i
	
	list(set = set, get = get, seti = seti, geti = geti)
}


## This functon will take a matrix that has already been run through makeCacheMatrix and first check to see if 
## the inverse of this matrix has already been calculated and cached. If so, it will return the cached inverse i.
## If not, then it will calculate the inverse of matrix x, return the inverse, and cache the inverse as i. 

cacheSolve <- function(x, ...) {
        i <- x$geti()
	if(!is.null(i)){
		message("Getting cached inverse matrix")
		return(i)
	}
	matrixdata <- x$get()
	i <- solve(matrixdata,...)
	x$seti(i) 
	return(i)
}
