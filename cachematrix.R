## This function just like the example one taken a matrix argument and tries to load the matrix and sets the values for its inverse such that the getinverse function 
## can return the cache of inverse matrix. The variable are similar to the mean example present just before this.

## This function have been tested for multiple matrices and found to be reliable. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL						## the value of inverse will be stored in this object only throughout the function
  set <- function(y) {				## takes the matrix and sets it.
    x <<- y
    m <<- NULL
  }
  get <- function() x				## Stores the matrix
  si <- function(solve) {m <<- solve}		
  gi <- function() {m}
  list(set=set,get=get,si=si,gi=gi)		## This list contains all the values.
}


## The function below tries to check if their is a cache copy of the inverse of the matrix if not then it tries to calculate the inverse of the matrix 
## and sets it to the cache function in setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$gi()
  if(!is.null(m)) {
    message("Already calculated here you go!")
    return(m)				##IMP: Don't use print() funtion in this place return() is perfect here.
  }
  data <- x$get()
  m <- solve(data, ...)
  x$si(m)
  m
}
