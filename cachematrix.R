## There are two functions which are being created. The first function
## "makeCacheMatrix" create a special type of matrix which will have its
## funtions calls to get, set the matrix and its inverse. The second function "cacheSolve"
## returns the inverse after computig it with the matrix input. However it does not 
## compute it but rathers gives it from the cache created by earlier function if the
## input matrix is repeated.

## This function creates a special matrix wiith parameters inverse, determinant
## and set of fucntions specific to that special matrix. The function list is returned 
## for this function.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	d <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	      d <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	setDeterminant <- function(determinant) d <<- determinant
	getDeterminant <- function() d 
	list(set = set, get = get, 
	setInverse = setInverse, getInverse = getInverse,
	setDeterminant = setDeterminant, getDeterminant = getDeterminant)
}


## This function first checks the stored determinant value for computation of inverse
## of the fucntion. If the stored value is anything other than 0, it goes ahead and 
## solves for inverse, in course of which if the input is a new matrix and not stored 
## in cache then it computes determinant of the matrix. If the determinant is non zero
## then it computes inverse of the matrix otherwise it displays a message rather than throwing
## error. 

cacheSolve <- function(x, ...) {
	
## Getting the determinant value of the special matrix.
	  determi <- x$getDeterminant()
## Checking if the stored determinant value is NULL or Non-zero for
## further computations.
	  if(is.null(determi) || determi != 0){
## Getting the inverse value of special matrix
        inv <- x$getInverse()
## Checking if the value stored for the inverse is not NULL
	  if(!is.null(inv)){
	  message("getting matrix from the cached memory")
	  return(inv)
	  }
## After above condition is false, implies it is a new matrix hence
## the function now starts computing.
	  data <- x$get()
	  dete <- det(data)
## Checks determinant value against zero to avoid any further computation.
	  if(dete == 0){
	  message("Determinant of your matrix is 0. Please input another one.")
	  x$setDeterminant(dete)
	  return(invisible(NULL))
	  }
	  inv <- solve(data)
	  x$setInverse(inv)
	  x$setDeterminant(dete)
	  return(inv)
## If the determinant value on computation is non zero the the else part 
## is executed.
	  }else{
	  message("Determinant of your matrix is 0. Please input another one.")
	  x$setDeterminant(dete)
	  return(invisible(NULL))
	  }
	  
}
