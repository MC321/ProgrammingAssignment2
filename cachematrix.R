## The following are a pair of functions to cache the 
## inverse of a matrix because matrix inversion is a costly computation,
## and it is beneficial to cache the inverse rather than computing it 
## repeatedly, for example in a loop.

## The first function makeCacheMatrix creates a special "matrix"
## object which can cache its inverse. This contains other functions within 
## namely, set,get,setinverse,and getinverse

makeCacheMatrix <- function(x = matrix()) {
 inv<-NULL
 set<-function(y){
  x<<-y
  inv<<-NULL
 }
 get<-function() x
 setinverse<-function(inverse) inv<<-inverse
 getinverse<-function() inv
 list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The second function cacheSolve computes the inverse of the special
## "matrix" created with the previous function. It first checks if the 
## inverse has already been calculated, if so, gets the inverse from cache 
## skipping the computation. If not, calculates the inverse and sets the 
## value of the inverse in cache via setinverse function.


cacheSolve <- function(x) {
 inv<-x$getinverse()
 if (!is.null(inv)){
  message("getting cached inverse")
  return(inv) ## Return a matrix that is the inverse of 'x'
 }
 data<-x$get()
 inv<-solve(data)
 x$setinverse(inv)
 inv 
}