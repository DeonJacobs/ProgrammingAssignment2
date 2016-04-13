## Date: 2016-04-13
## Coursera R PRogramming module
## Two functions implemented: 
## 1. makeCacheMatrix - special function using lexical scoping to save the calculation of the inverse matrix operation 
## from the matrix passed as argument to the function.
## 2. cacheSolve - function taking the above matrix object as an argument and determines if the inverse of the matrix
## should be calculated by reading the cached variable via the passed object. The inverse matrix calculation only executes
## the matrix is square

## Special matrix function to set,get matrix, and store matrix inverse calculation using lexical scoping
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y                                       #Set new matrix value 
    m <<- NULL                                    #When new matrix is set, previous inverse calculation to be nulled
  }
  get <- function() x                             #Return the matrix
  setinverse <- function(inverse) m <<- inverse   #Set passed inverse to cache
  getinverse <- function() m                      #Get cached inverse matrix
  list(set = set, get = get,                      #Return list of function objects
       setinverse = setinverse,
       getinverse = getinverse)
}


## Functin calculating matrix inverse if not already done so by recalling cached inverse matrix variable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()                             #get inverse matrix from cache
  if(!is.null(m)) {                               #if cached inverse matrix null: Not stored, continue to calculate new inverse
    message("getting cached inverse matrix")
    return(m)                                     #inverse matrix calculated, so exit function
  }
  #not calculated, so do inverse calculation
  data <- x$get()
  #check that the matrix is square
  matdim <- dim(data) #get matrix dimensions
  if(matdim[1]==matdim[2])
  {
    m <- solve(data, ...)                          #calculate the matrix inverse  
  }
  else{
    message("matrix not square!")
    return()
  }
  #update the inverse matrix calculation result to the cache
  x$setinverse(m)
  m
}
