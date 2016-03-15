## I made minor modification to the functions in the description of the programming assignemnt
## These minor modifications makes it possible to cache the inverse of the a matrix 
## instead of the mean of a vector

## This function creates a special list, which is capable of cache the inverse aswell

makeCacheMatrix <- function(x = matrix()) {
  # Making sure that "i" is set to NULL
  i <- NULL
  
  # This function adds the data to the object
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # This function gets the data from the object
  get <- function() x
  
  # These functions store the inverse and make it accessible
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  # Compipling into a list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates the inverse or returns the previously cached inverse matrix

cacheSolve <- function(x, ...) {
  # Getting inverse matrix if it exists in the object
  i <- x$getInverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # Otherwise gets the data from the object, calculates the inverse and caches it
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}


## Test if the result is what required
test <- matrix(c(7, 2, 1, 0, 3, -1, -3, 4, -2), ncol = 3, byrow = T)
test

testMat <- makeCacheMatrix(test)

cacheSolve(testMat)
cacheSolve(testMat)
