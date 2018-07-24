#This pair of functions take a matrix, calculate its inverse, and
#caches the result.

#This function creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   #initializes "inv" object
  set <- function(y) {   #This function sets 
    x <<- y              #matrix y as x in the parent environment
    inv <<- NULL         #and clears the "inv" object.
  }
  get <- function() x    #retrieves matrix, x, from makeCacheMatrix environment
  setInverse <- function(inverse) inv <<- inverse   #sets the inverse in the makeCacheMatrix environment
  getInverse <- function() inv   #retrieves inverse matrix, inv, from makeCacheMatrix environment
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
  #assigns each of these functions to an element within a list and returns it to the parent environment.
}

#This function calculates the inverse of the cached matrix, or retrives it if it's cached.
#Requires an input argument of type makeCacheMatrix()

cacheSolve <- function(x, ...) {
  #Checks if there is a cached inverse of matrix x and, if so, returns it.
  #Otherwise, calculates and returns a matrix that is the inverse of x
  inv <- x$getInverse()   #loads cached inverse into inv object
  if(!is.null(inv)) {                   #If there is a cached inverse in x, then returns it.
    message("getting cached data")
    return(inv)
  }
  #If inverse isn't cached, these lines calculate it.
  data <- x$get()  #loads matrix from x.
  inv <- solve(data, ...)  #solves for inverse of matrix.
  x$setInverse(inv)   #caches the newly inverted matrix.
  inv   #returns newly inverted matrix
}
