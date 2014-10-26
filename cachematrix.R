##  Two functions, makeCacheMatrix and CacheSolve.  makeCacheMatrix
##  creates a special matrix object that will cache its inverse
##  cacheSolve will comput the inverse of the matrix.  The inverse
##  is calculated using the solve(x) function


##  makeCacheMatrix accepts a matrix from the user and creates a matrix that 
##  can be cached.  It has 4 sub-functions, setMatrix, getMatrix, getInverse, and
##  setInverse. getinverse and setInverse use cacheSolve. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setInverse<-function(Inverse){
    m<<-Inverse
  }
  getInverse<-function(){
    m
  }
    
  list(setMatrix=setMatrix,getMatrix=getMatrix,getInverse=getInverse,setInverse=setInverse)

}

## cacheSolve returns the the inverse of the user supplied matrix.  It returns two
## functions, getInverse, which displays the inverse, and set inverse, which sets the inverse.

cacheSolve <- function(x, ...) {
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$getMatrix()
  m<-solve(data)
  x$setInverse(m)
  m
 
}

