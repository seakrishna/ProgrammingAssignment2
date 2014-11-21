
## makeCacheMatrix creates a matrix the can 
##have the set and get methods to get the value and set the value
##It also has a method to get the inverse of the matrix

##CacheSolve is a function that if the inverse is not existent then it
## calculates the inverse
##If already calculated then it uses the cached value.

## This is a function that is used to create a matrix that can be used to
##set the values and get values and calculate inverse
makeCacheMatrix <- function(x = matrix()) { 
  ##Set the inverse to null
  inverse <- NULL
  
  ##Create the set function to set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## This will return the matrix x
  get <- function() x
  
  ## Can be used to set the inverse of the matrix. 
  ##It uses solve ot create the inverse and stores it in inverse
  setInverse <- function(solve) inverse <<- solve
  
  ##Returns the inverse of the matrix
  getInverse <- function() inverse
  
  ##list of all the methods that can be called on the matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
} 


## Returns the iverse of a matrix. If already computed does not 
##recompute and returns the cached value.
cacheSolve <- function(x, ...) { 
  ##Call the get inverse function to calculate inverse
  inverse <- x$getInverse()

  ##if value of inverse is not null then it has been computed.
  ##return the caced precomputed value.
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ##If not precomputed compute for the first time. 
  ##get the matrix x
  data <- x$get()
  ##get the inverse of the data
  inverse <- solve(data, ...)
  ##Set the inverse 
  x$setInverse(inverse)
  
  ##return inverse
  inverse
  
} 
