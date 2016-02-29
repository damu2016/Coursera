## R Programming Week 3 
## Assignment 2: Lexical Scoping--caching the inverse of a matrix

## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## makeCacheMatrix function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
  x<<-y
  inv<<-NULL
}
get<-function() x
setInverse<-function(inverse) inv <<- inverse
getInverse<-function() inv
list(set=set, 
     get=get,
     setInverse=setInverse,
     getInverse=getInverse)
}

## cacheSolve function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated and the 
## matrix has not changed, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the invers of 'x'
    inv<-x$getInverse()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    data<-x$get()
    inv<-solve(data, ...)
    x$setInverse(inv)
    inv
}
