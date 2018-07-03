##This function Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
## get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object

## can cache its own object.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y){
  x <<- y
  m <<- NULL
}
get <- function() x
setsolve <- function(solve) m <<- solve
getsolve <- function() m
list(set = set,get = get,setsolve = setsolve,getsolve = getsolve)
 }


## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an input and checks inverse matrix from makecachematrix function
##In case m from makeCacheMatrix((matrix) is empty, it gets the original matrix data from and set the invertible  matrix by using the solve function
##In case m from makeCacheMatrix((matrix) has some value in it,it returns a message  "Getting Cached Invertible Matrix" 
cacheSolve <- function(x, ...) {
    m <- x$getsolve() 
    if(!is.null(m)) {
      message("getting cached data")
      return(m)## Return a matrix that is the inverse of 'x'
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
    
