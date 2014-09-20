## These two functions do timing and unit tests
## of the cachedMatrix functions.

## This function was used to test several possible
## approaches to finding if two matrices are equal.
## Approaches:
##    The naive approach of looping over rows and
##     columns was immediately rejected and not tested.
##
##    The second approach was to test row-wise taking
##    advantage of R's vectorization but potentially
##    rejecting before the full matrix was tested.
##
##    The third approach was to do an "==" check
##    on the two matrices.
##
##    The fourth approach was to do a matrix subtraction
##    and then check for any differences bigger than
##    two eps(ilon).   
##
## Results:    
##
##  The row-wise approach was factor of 10 or more
##  slower than the other approaches and was rejected.
##
##  The check of matrix equality and the subtraction
##  approach were about the same in terms of time cost.
##  Given that time result the subtractive method was
##  chosen for two reasons.  The first is that the
##  equality check still needed an additional post
##  processing since the result was on an element
##  and not matrix level.  The second reason is that
##  the equality check was for true equality and would
##  fail for differences at the eps level accoring
##  to the documentation.  
##  
##

timingTests <- function(){
  
  # performs several timing tests to see
  # which is the most efficient way to 
  # determine if two matrices are identical
  
  # check to see if nrow and ncol are same for new and old matrices
  # 1) check if sizes are same
  # 
  A=replicate(2000,rnorm(2000));
  B=replicate(2000,rnorm(2000));
  C<-A;
  
  # Start the clock!
  print("by rows...");
  ptm <- proc.time();
  for(i in 1:nrow(A)){ 
    rowi = A[i,] - B[i,]; 
    maxrow=max(rowi); 
    minrow = max(-1*rowi); 
    res[i]<-max(maxrow,minrow);   
  }
  # Stop the clock
  time1<-proc.time() - ptm;
  print(time1);
  
  # Start the clock!
  print("equality...");
  ptm <- proc.time();
  truth<-(A==B);
  # Stop the clock
  time2<-proc.time() - ptm;
  print(time2);
  
  
  
  # the following clocked faster than row-by-row
  # checking.  Most likely due to the vectorized
  # internals of R
  
  
  # Start the clock!
  print("subtraction...");
  ptm <- proc.time();
  res<-(A-B);
  maxis<-max(res);
  mini<-min(res);
  # Stop the clock
  time2<-proc.time() - ptm;
  print(maxis);
  print(mini);
  if(abs(mini) > (2*.Machine$double.eps)){
    equality<-FALSE;
  }
  if(abs(maxis) > (2*.Machine$double.eps)){
    equality<-FALSE;
  }
  
  print(equality);
  print(time2);
  
  # Start the clock!
  print("subtraction...");
  equality<-TRUE;
  ptm <- proc.time();
  res<-(A-C);
  maxis<-max(res);
  mini<-min(res);
  if(abs(mini) > (2*.Machine$double.eps)){
    equality<-FALSE;
  }
  if(abs(maxis) > (2*.Machine$double.eps)){
    equality<-FALSE;
  }
  
  
  # Stop the clock
  time2<-proc.time() - ptm;
  print(maxis);
  print(mini);
  print(equality);
  print(time2);
}

## this function performs testing of the
## makeCacheMatrix and cacheSolve functions.  
cachedMatrixTests <-function(){
  
  source("cachematrix.R");
  
  # make a 10x10 random matrix
  baseMatrix<- replicate(10,rnorm(10)) ;
  cachedTestMatrix <- makeCacheMatrix(baseMatrix);
  
  print("get()...")
  print(cachedTestMatrix$get());
  
  print("getinverse()...")
  print(cachedTestMatrix$getinverse());
  
  inverseMatrix <- solve(cachedTestMatrix$get());
  
  print("computed inverse")
  print(inverseMatrix);
  
  cachedTestMatrix$setinverse(inverseMatrix);
  
  print("getinverse()...")
  savedInverse<-cachedTestMatrix$getinverse();
  print(savedInverse);
  
  # test set with identical matrix
  print("Inverse should be non-NULL...")
  test2<-baseMatrix;
  cachedTestMatrix$set(test2);
  print(cachedTestMatrix$getinverse());
  
  # set with a different matrix
  print("Inverse should be NULL...")
  baseMatrix2<- replicate(10,rnorm(10)) ;
  test3<-baseMatrix2;
  cachedTestMatrix$set(test3);
  print(cachedTestMatrix$getinverse());
  
  # check error condition
  baseMatrix3 <- replicate(5, rnorm(10));
  cachedTestMatrix$set(baseMatrix3);
  
  #check similar matrices
  cachedTestMatrix$set(baseMatrix);
  inverseMatrix <- solve(baseMatrix);
  cachedTestMatrix$setinverse(inverseMatrix);
  baseMatrix4<-baseMatrix + .Machine$double.eps
  cachedTestMatrix$set(baseMatrix4);
  print("Inverse should be non-NULL...")
  print(cachedTestMatrix$getinverse());
  
  baseMatrix4<-baseMatrix + 2*.Machine$double.eps
  cachedTestMatrix$set(baseMatrix4);
  print("Inverse should be non-NULL...")
  print(cachedTestMatrix$getinverse());
  
  baseMatrix4<-baseMatrix + 3*.Machine$double.eps
  cachedTestMatrix$set(baseMatrix4);
  print("Inverse should be  NULL...")
  print(cachedTestMatrix$getinverse());
 
  #Note:  to use these two tests uncomment the print 
  # statement in cachematrix.R, the cacheSolve function.
  # pass a matrix with a NULL cached inverse
  print("This should cause a solve...")
  inverseResult <- cacheSolve(cachedTestMatrix);
  print(inverseResult);
  # pass the same matrix and verify it doesn't solve
  print("This should NOT cause a solve...")
  inverseResult<-cacheSolve(cachedTestMatrix);
  
  
}

