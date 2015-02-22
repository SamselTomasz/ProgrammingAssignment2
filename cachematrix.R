## Functions that cache the invers of a matrix

## Creates matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	
	## Some initialization needed 
	InversedMatrix <- NULL
	
	## Method setting the matrix
	set <- function(matrix){
		m <<- matrix
		## If different data set was passed on, NULL the inverse.
		i <<- NULL
	}

	## Method that get the matrix
	get <- function(){
		m
	}

	## Method that set inversed matrix
	setInverse <- function(inverse){
		i <<- inverse
	}

	## Method that get inversed matrix
	getInverse <- function(){
		i
	}

	## List of the methods that can be used
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Compute the inverse of the matrix. If the inverse has already
## been calculated, just return the inverse from cache.
cacheSolve <- function(x, ...) {
        ## Get the value of inverse
	m <- x$getInverse()

	## Check if inverse is calculated already, if so
	## return it and terminate the function

	if(!is.null(m){
	   message("getting cached data")
	   return(m)
	}
	
	## If the inverse is not stored, the function continue
	## and will calculate it.
	
	## Get the matrix to work on
	data <- x$get()

	## Calculate the inverse, as mentioned at 
	## stackoverflow.com/questions/11995832/inverse-of-matrix-in-r
	m <- solve(data)%%data

	## and set the inverse 
	x$setInverse(m)

	## return the matrix
	m
}

