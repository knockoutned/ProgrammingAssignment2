## This function takes a matrix and stores the inverse for future computations.
## This saves time and processing power.

## makeCacheMatrix creates a list of four functions
## set the value of a matrix
## get returns the matrix
## setinverse computes the inverse
## getinverse returns the inverse

makeCacheMatrix <- function(x = matrix()) {  ##This creates a blank matrix x
   inv <- NULL  ##Sets the default value for m within the function equal to NULL 
   set <- function(y) {
      x <<- y  ##Sets the value of y outside of this function equal to x
      inv <<- NULL ##Sets the value of inv outside of this function equal to NULL
   }
   get <- function() x ##Assigns value for function 'get' equal to the input matrix
   setinverse <- function(solve) inv <<- solve ##Assigns values for 'setinverse', and 'inv' in the outside environment
   getinverse <- function(solve) inv ##Returns inv (which is NULL in this case)
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix, but tests if the inverse has already been calculated.
## If it has, it moves on. If it has not, it calculates it.

cacheSolve <- function(x, ...) {
   inv <- x$getinverse() ##Sets inv equal to value of getinverse function (which is NULL)
   if(!is.null(inv)) {  ##Conditional: if not equal to NULL (has the inverse already been calculated)
      message ("getting cached data.")
      return(inv)
   }
   data <- x$get() ##Sets data equal to the results of get() function
   inv <- solve(data) ##Calculates the inverse of data (which equals x) and stores it as inv
   x$setinverse(inv)  ##Stores inverse value in the cache using the setinverse function
   inv
}

