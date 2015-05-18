#makeCacheMatrix returns a list
#composed of functions for getting and setting
#the matrix 'x' and its inverse. These functions are
#defined within the environment both 'x' and
#'inverse' are declared, so they are still able to
#access or mutate these objects (with the help of <<-).

#cacheSolve takes as parameters a list returned
#by "makeCacheMatrix" as well as additional
#parameters specified by '...' that may be used
#in the call to 'solve'. If the inverse was already
#solved for this matrix, it returns it from the 'cache' (or closure).
#Otherwise, it computes the inverse and returns it.


#Returns an object that stores a matrix as well
#as its inverse.
makeCacheMatrix <- function(x = matrix()) {
   inverse <- NULL
   set <- function(y) {
      x <<- y
      inverse <<- NULL
   }
   get <- function() x
   setinverse <- function(i) inverse <<- i
   getinverse <- function() inverse
   list(set=set, 
        get=get,
        setinverse=setinverse, 
        getinverse=getinverse)
}

#Solves the inverse of the passed in matrix
#object if it isn't already cached. Otherwise, returns
#the cached matrix inverse.
cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
   if (!is.null(inv)) {
      message("returning stored inverse!")
      return(inv)
   }
   my_matrix <- x$get()
   inv <- solve(my_matrix, ...)
   x$setinverse(inv)
   inv
}
