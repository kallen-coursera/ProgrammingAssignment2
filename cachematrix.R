## This function contains the solution to Programming
## Assignment no.2 for the R Programming Coursera class.

## This function creates a special matrix that stores a 
## matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <-function()x
  set_inverse<-function(matrix_inverse)m<<-matrix_inverse
  get_inverse<-function()m
  list(set=set, get=get,
      set_inverse=set_inverse,
      get_inverse=get_inverse)

}


## This function returns the inverse of a matrix.
## If this is the first time it is called, it calculates
## the inverse, stores it, and returns it. On subsequent
## calls, it returns the cached value.

cacheSolve <- function(x, ...) {
  m<-x$get_inverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$set_inverse(m)
  m
}
