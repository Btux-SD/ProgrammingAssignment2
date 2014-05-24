## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix and contains 2 derived functions from makeCacheMatrix that
## will be used in the next function cacheSolve and are used in makeCacheMatrix to do a initial set.

makeCacheMatrix <- function(x = matrix()) {
    s<-NULL
    set<-function(y){
        x<<-y
        s<<-NULL
    }
    get<-function() x
    setsolve<-function(solve) s<<- solve
    getsolve<-function() s
    list(set=set, get=get,
         setsolve=setsolve,
         getsolve=getsolve)
}

## This function calculates the inverse ( with the function "solve"). First it checks if
## the solve has already been applied.

cacheSolve <- function(x, ...) {
    s<-x$getsolve()
    if(!is.null(s)){
        message("You are getting cached data :-)")
        return(s)
    }
    matrix<-x$get()
    s<-solve(matrix, ...)
    x$setsolve(s)
    s
}
