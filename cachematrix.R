## These functions cache the inverse of a matrix, instead of calculating it repeatedly, which can be costly. 

## takes an invertible matrix and returns a list containing functions that set the matrix, get the latest stored matrix,
## set the inverse of the matrix and get the latest stored inverse respectively

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
    
  }
  
  get <- function(){
    x
  }
  setInverse <- function(inverse){
    inv <<- inverse
  }
  getInverse = function(){
    inv
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Takes the result of makeCacheMatrix(), i.e. a list of functions
## checks whether the inverse is already stored using x$getInverse(). 
## If so, returns the stored inverse without calculating
## again. Otherwise, get the stored matrix using x$get(), 
## calculates inverse using the in built solve() function
## and stores the calculated inverse and returns it

cacheSolve <- function(x,...){
  final_inverse = x$getInverse()
  
  if(!is.null(final_inverse)){
    message("getting cached data")
    return(final_inverse)
  }
  
  data <- x$get()
  
  final_inverse <- solve(data,...)
  x$setInverse(final_inverse)
  final_inverse
}
