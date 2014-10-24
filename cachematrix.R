## makeCacheMatrix - creates a matrix
## cacheSolve - returns the inverse of the matrix (returns the cached value if already cached).

##makeCacheMatrix creates a special "matrix", which is a matrix containing a function to
##set the matrix
##get the matrix
##set the inverse of the matrix
##get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setinverse<-function(inverse) m<<- inverse
getinverse<-function() m
list(set=set, get=get,
   setinverse=setinverse,
   getinverse=getinverse)
}

## cacheSolve calculates the inverse of the matrix created with the above function
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the 
## inverse in the cache via the setinverse function.

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getinverse()
    if(!is.null(m)){
      message("getting cached data")
       ## Return a matrix that is the inverse of 'x'
      return(m)
    }
    data<-x$get()
    m<-solve(data, ...)
    x$setinverse(m)
    m
}

## Unit test - Explanation

## source("cachematrix.R")

## matrix1 <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
##
## matrix1$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##
## cacheSolve(matrix1) ## calculates the inverse
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##matrix1$getinverse()
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
##cacheSolve(matrix1)
##getting cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## matrix1$set(matrix(c(5,6,7,8), nrow=2, ncol=2))
## matrix1$get()
##     [,1] [,2]
##[1,]    5    7
##[2,]    6    8
##cacheSolve(matrix1)
##     [,1] [,2]
##[1,]   -4  3.5
##[2,]    3 -2.5
##matrix1$getinverse()
##     [,1] [,2]
##[1,]   -4  3.5
##[2,]    3 -2.5
##cacheSolve(matrix1)
##getting cached data
##     [,1] [,2]
##[1,]   -4  3.5
##[2,]    3 -2.5