## Put comments here that give an overall description of what your
## The following pair of the functions is to compute inverse matrix and cache it 
## so that it does not need to compute repeatedly the same inverse matrix.

## functions do

## Write a short comment describing this function

##The following function create a list containing 4 functions:
##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse matrix
##4.  get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinv<-function(inv) i<<-inv
  getinv<-function() i
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
##The following function gives the inverse of the matrix created with the above function. 
##It first checks to see if the inverse has already been calculated and if so, 
##it retlieve the inverse from the cache and skips the computation. Otherwise, it calculates the 
##inverse of the matrix and sets the inverse in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrix<- x$get()
  i <- solve(matrix, ...)
  x$setinv(i)
  i
 }
