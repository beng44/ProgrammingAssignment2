##This program receives a matrix, creates the inverse and caches it, then checks to see if the inverse has been taken and returns the inverse matrix.

## This function receives a matrix input, creates the inverse matrix, and caches the inverse matrix

makeCacheMatrix <- function(x = matrix()) { #Only receives numeric inputs
  m <- NULL #This block of code sets the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x #This gets the matrix
  setinverse <- function(solve) m <<- solve #This solves the inverse for the matrix
  getinverse <- function() m #Sets the value for the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function retrieves the cached matrix, checks to see
## if the matrix is an inverse, and then returns the 
## inverse of the original matrix.

cachesolve <- function(x, ...) {
  m <- x$getinverse() #Retrieves the data from the first function
  if(!is.null(m)) { #Checks to see if the data has already been manipulated into an inverse
    message("getting cached data")
    return(m)
  }
  data <- x$get()#Solves for the inverse in the case that m has not been solved yet.
  m <- solve(data,...)
  x$setinverse(m)
  m #returns m
}
