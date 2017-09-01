## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object
##  that can cache its inverse.
##  This function will- set the value of the matrix
## -get the value of the matrix
##- set the value of the inverse of the matrix
##  - get the value of the inverse of the matrix
 ## The function also assumes that all the matrices are invertible 

makeCacheMatrix <- function(x = matrix()) {
				mat_inv <- NULL
				set <- function(y){
						x<<-y
						mat_inv <<-NULL
				}
				get <- function() x
				setinverse <- function(inverse) mat_inv <<-inverse
				getinverse <- function() mat_inv
				list(set = set, get = get, setinverse =setinverse, getinverse= getinverse)

}


## Write a short comment describing this function
## his function computes the inverse of the special
##    "matrix" returned by `makeCacheMatrix` above. If the inverse has
##    already been calculated (and the matrix has not changed), then
##   `cacheSolve` should retrieve the inverse from the cache.
## The function also assumes that all the matrices are invertible 

cacheSolve <- function(x, ...) {
        mat_inv <- x$getinverse()
		if(!is.null(mat_inv)){
			message("getting cached inverse of the matrix")
			return(mat_inv)
		}
		data <-x$get()
		mat_inv <-solve(data)
		x$setinverse(mat_inv)
		mat_inv
		
		
}
		

