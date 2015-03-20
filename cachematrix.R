## Calculating the inverse of a Matrix using a cached matrix


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinvertmatrix<-function(invertmatrix) m<<- invertmatrix
  getinvertmatrix<-function() m
  list(set=set, get=get,
       setinvertmatrix=setinvertmatrix,
       getinvertmatrix=getinvertmatrix)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinvertmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinvertmatrix(m)
  m
}
