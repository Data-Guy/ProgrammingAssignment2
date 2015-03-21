## Put comments here that give an overall description of what your
## functions do

## The following two functions work together. The first creates a function (makeCacheMatrix)
## that receives a "matrix" as an input and sets up other functions and variables that are used
## in cacheSolve.The cacheSolve function uses the funcitons and variables from makeCacheMatrix
## to create an inverse of the matrix provided in makeCacheMatrix. To do this, cacheSolve first
## determines if the inverse matrix has already been calculated. If it has, then cacheSolve retrieves
## it and passes it as the results. If the inverse matrix has not been calculated, cacheSolve
## calculates it and then passes/stores the result.

## There are two key points here. First, caching the values can save time in running code in R. This
## can be easily verified here by using a different function (not written here) to capture the
## execution time of cacheSolve when the same matrix value is passed to it; the second execution will
## be faster especially if the matrix is large. The other point most relevant to this exercise is
## that these functions keep track of which "environment" variables are set in. This makes sure that
## different values can be passed to the functions and the calculations are correct. This is known
## as lexical scoping and the "<<-" parameter is use in these functions to do this.


##Write a short comment describing this function

## makeCacheMatrix is a funciton that takes a matrix as its input. The function "sets up" local 
## environment variables (x and m) and creates four funcitons: set, get, setmatrix, and getmatrix.
## These functions are used in the next function, cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      get<-function() x
      setmatrix<-function(solve) m<<- solve
      getmatrix<-function() m
      list(set=set, get=get,
           setmatrix=setmatrix,
           getmatrix=getmatrix)
}


## Write a short comment describing this function

## cachesolve is a function that resolves the inverse of a square matrix (e.g. 2x2, 3x3, etc.). Before
## it does this the fuction fist checks to see if the inverse of the function's argument has already
## been calculated (if(!is.null(m))). If it has, the function will tell you that it is using cached
## data and then return the inverse matrix. If not, cacheSolve solves the inverse (m<-solve(matrix, ...)),
## sets the value of "m" to the solved inverse so that if called again the cache will be used, and then
## returns the inverted matrix m as an output.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m<-x$getmatrix()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      matrix<-x$get()
      m<-solve(matrix, ...)
      x$setmatrix(m)
      m
}
