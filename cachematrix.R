
# makeCacheMatrix receives a matrix variable, and sets variables and functions in memory, 
# and returns a list of functions nested within makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL                                                         ## Set m=NULL
  set<-function(y){                                               ## Create set fiunction to store the matrix
    x<<-y                                                         ##put the initial matrix into cache as x
    m<<-NULL
  }
  get<-function() x                                               ## Create function to get/return the matrix passed
  setmatrix<-function(solve) m<<- solve                           ## Create function to set the value of setmatrix in cache to the value of m  getmatrix<-function() m
  list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)  
}


# cacheSolve function receives a variable that is a matrix 
# cacheSolve returns the inverted form of the submitted matrix.
# CacheSolve, when called, checks to see if there already exists a non-NULL value for m in cache.
# If cacheSolve finds a non-NULL value for m existing in cache already, it returns the value found
# If cacheSolve does not find an existing non-NULL value for m in cache, cacheSolve gets the commandline values for m, inverts the matrix 
# in m, and sets the value of m in the cache environment.

# cacheSolve then evaluates the ending matrix so as to return it.

cacheSolve <- function(x=matrix(), ...) { ##receive cachematrix
  m<-x$getmatrix()                        
  if(!is.null(m)){                        ##check to see if m is null
    message("getting cached data")        ##If m!=NULL, show message geting chache data
    return(m)
  }
  matrix<-x$get                           ## Call the nested function x$get in makeCacheMatrix to obtain the non-inverted matrix
  m<-solve(matrix, ...)                   ## Use solve() to invert 
  x$setmatrix(m)                          ## call nested function x$setmatrix() to set m in cache enviroment
  m                                       
}
