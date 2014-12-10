## To avoid the computation of inverse of matrix more than once, we first define a function to set
## and get the matrix and its inverse. It takes a matrix and return a list to calculate the inverse.
##Next step is giving this list to another function for calculating the inverse. 

## 1-It sets the matrix value.2- It gets the value for matrix.3-It sets the inverse of the matrix
## 4-It gets the matrix inverse. It returns a list of all these componets

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function gets a list which is returened by makeCacheMatrix. It calculates the inverse of 
## matrix. It first checks if the matrix inverse has been already computed. if so it gets it from 
##cache and skip computation.Otherwise it calculates the inverse and set the inverse using
##setinverse function defined in the previous function 

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I<-solve(data)
  x$setinverse(I)
  I
}