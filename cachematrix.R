## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # this is the inverse that will be cached
  cachedInverse <-NULL;
  
  # returns the actual matrix data that
  # is stored in this object
  get<-function(){
    return( x );
  }
  
  # sets the matrix data in the function
  # to be equal to newMatrix
  # NULL's the inverse so that we know
  # to recompute it
  set<-function( newMatrix = matrix() ){
    if( nrow(newMatrix) == ncol(newMatrix) ){
      if(matricesIdentical(newMatrix) == FALSE ) 
      {
          # matrices are different  
          cachedInverse <<- NULL;
          x <<- newMatrix;  
      }
    }
    else{
      # invalid input!
      message("Warning!  Input matrix is not square!");
    }
  }
  
  # determines if two matrices are the same
  matricesIdentical<-function(y){

    equality<-TRUE;
    
    #if the matrices are different sized....
    if( (numcols() != ncol(y)) ||
          (numrows() != nrow(y)) ){
      equality <- FALSE;
      return (equality);
    }
    
    # do subtraction and verify that
    # all elements match to < 2 eps
    res<-(y - x);
    maxi<-max(res);
    mini<-min(res);
    if(abs(mini) > (2*.Machine$double.eps)){
      equality<-FALSE;
    }
    if(abs(maxi) > (2*.Machine$double.eps)){
      equality<-FALSE;
    }
    
    return (equality);
  }
  
  
  # returns the cached inverse
  getinverse <- function(){
    return (cachedInverse);
  }
  
  # this sets the new matrix inverse
  setinverse <- function(newInverse){
    cachedInverse <<- newInverse;
  }
  
  # these two functions return the
  # size of the matrix.  
  numcols<-function(){
    return( ncol(x));
  }
  
  numrows<-function(){
    return( nrow(x));
  }
  
  # the function returns a list with the
  # set and get functions
  return(
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # if the cached inverse is NULL, then solve
  inverse <-x$getinverse()
  if(length(inverse) == 0){
    # using the standard "solve" function
    # for a square matrix
    #print("solving....");    # for unit testing only
    x$setinverse(solve(x$get()));
  }
  
  # either way, we get the correct 
  # inverse from this call
  return (x$getinverse());
}
