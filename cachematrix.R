## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix, which return to us a list containing 
## functions that set and get the values of the matrix and of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	## We initiate our function defining inv as NULL.
	## inv is the variable responsable for bin the result of inverse matrix
	inv <- NULL
	set <- function(y){
		## Here we attribute to "x" the value (matrix) in y if the current matrix is different from the previous
		x <<- y
		inv <<- NULL
	}
	## save the matrix in a function that return x
	get <- function() x ##get the value of matrix x
	setinv <- function(aux_inv) inv <<- aux_inv ## set the value of inverse 
	getinv <- function() inv ## get the value of inverse
	list(set = set, get = get, setinv = setinv, getinv = getinv) ##object that return to us (we can access their elements using x$name)
}


## Write a short comment describing this function

## This function take the result of previous function and calculates the inverse if the current matrix is a new input. If 
## the matrix has been calculate, the function return the value contained in the returned by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  ## Here we will attribute to "inv" the value contained in the list returned by previous function.
	  inv <- x$getinv()
	  ## Checking if inv has been calculated (to the current matrix) and if the answer were True, then
	  ## a message and a previous inv are returned by the code
	  if(!is.null(inv)){
		  message("getting cached data")
		  return(inv)
	  }
	  ## If the condition above is not satisfied, then the calculation of the inverse matrix is done and 
	  ## the result is returned in the end of the process.
	  data <- x$get()
	  inv <- solve(data, ...)
	  x$setinv(inv)
	  inv
}
