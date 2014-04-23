## The set of functions will create a special matrix that can cache the inverse of a matrix
## as well as compute the inverse of the special matrix.

## This function will create a special matrix that can cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  a<- NULL
  set<-function(y){
    
    x<<-y
    a<<-NULL
  }
  
  get<-function() x
  setmatrix <- function(solve) a <<- solve
  getmatrix<-function() a 
  list(set=set,get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## This function computes the inverse of the special matrix provided by the makeCacheMatrix function.
## If the inverse has already been calculated, the function should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  
  a <- x$getmatrix()
  if(!is.null(a)){
    message("getting cahched data")
    return(a)
  }
  
  data<-x$get()
  a<-solve(data,...)
  x$setmatrix(a)
  a
  
}


b<-makeCacheMatrix(matrix(1:4,2))
cacheSolve(b)
