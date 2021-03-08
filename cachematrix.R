## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # create an inverse object that can cache its inverse
  inverse <- NULL
  
  # store the past and re-init the inverse
  # the "<<-" operator used to set variable that already exists 
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  # get the value of the matrix.
  get <- function() x
  
  setinverse <- function(inv){
    # set the value of the inverse matrix
    inverse <<- inv
  }
  
  # get the value of the inverse matrix
  getinverse <- function() inverse
  
  # create the list to access properties with $ sign. 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # get the inverse from the special matrix
  inverse <- x$getinverse()
  
  # if cache already, return it
  if(!is.null(inverse)){
    message("getting cache data")
    return(inverse)
  }
  
  # get the matrix from the object
  data <- x$get()
  
  # inverse the matrix using solve
  inverse <- solve(data, ...)
  
  # cache the result for the later usage
  x$setinverse(inverse)
  
  # return the result
  inverse
}

## test
 m <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
 cacheSolve(m)
 cacheSolve(m)

 