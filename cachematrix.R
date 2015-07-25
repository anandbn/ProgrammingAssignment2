######################################################################
## This R File Contains two functions
##
## makeCacheMatrix() - To create a matrix that caches inverse
##
## cacheSolve()      - To return an inverse of a matrix
##
######################################################################
## makeCacheMatrix()
## 
## Function that contructs a matrix and decorates it with additional
## functions that caches the inverse of the matrix that passed in 
## as a parameter or a new empty matrix. The function & usage is 
## below:
##   set        : Set the matrix that needs to be cached
##   get        : Get the original matrix 
##   setInverse : Set the inverse matrix value
##   getInverse : Get the current value of matrix value
##
## To contruct a empty cache matrix
##
##      cacheMatrix <- machCacheMatrix()
##
## To contruct a cache matrix from a 2x2 matrix
##
##      param=matrix(c(1,2,3,4),byrow = TRUE,nrow=2,ncol=2)
##      cacheMatrix <- machCacheMatrix(param)
######################################################################
makeCacheMatrix <- function(x = matrix()) {
  
  # Reset the value of theInverse matrix as NULL
  theInverse <- NULL
  
  #Set function that assigns the original matrix
  set <- function(y) {
    
    # Setting value of 'x' to the matrix passed via 'y'
    x <<- y
    
    #Resetting inv matrix to NULL
    theInverse <<- NULL
  }
  
  #Returns the value of orginal matrix 'x'
  get <- function() x
  
  # setting the 'theInverse' to the inverse matrix 
  # value passed in the parameter
  setInverse <- function(inv) theInverse <<- inv
  
  # returns the current value of 'theInverse'
  getInverse <- function() theInverse 
  
  # Return the functions defined as a list that
  # decorates the matrix 'x' with additional 
  # functions
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  

}


######################################################################
## cacheSolve()
## 
## Function that returns the inverse matrix.The function returns 
## directly from the cache if previously computed. If not previously 
## cache it inverses the matrix and then returns the inversed matrix
######################################################################
cacheSolve <- function(x, ...) {
  
  # Get the Mean from the passed in vector
  inv <- x$getInverse()
  
  # If it's not null, then just return 
  # the mean cached
  if(!is.null(inv)) {
    message("Getting cached inverse matrix")
    return(inv)
  }
  
  # Else get the original matrix using $get
  theMatrix <- x$get()
  
  # Caculate the inverse of the matrix
  inv <- solve(theMatrix)
  
  # Set the inverse back to the vector using $setInverse
  x$setInverse(inv)
  
  # Return the inverse matrix 
  inv
}
