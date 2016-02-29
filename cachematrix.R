## The following functions work together to cache a matrix and its inverse.
## see cacheSolve.test() for usage example.


#' makeCacheMatrix factory pattern utility for caching matrix and its inverse
#'  feaux class with constructor: cache <- makeCacheMatrix(myMatrix)
#'  see cacheSolve.test() for usage example.
#'  note: cachedMatrix and cachedInverseMatrix scope follows makeCacheMatrix instance.
#'
#' @param cachedMatrix 
#'
#' @return caching utility for matrices and their inverses
#'
#' @examples see cacheSolve.test
#' 
makeCacheMatrix <- function(cachedMatrix) {
  
  ## constructor sets emtpy values in .GlobalEnv$globalMatrixCache
  if (!exists("MatrixCacheIndex", envir = .GlobalEnv)) {
    .GlobalEnv$MatrixCacheIndex <- 1;
  } else {
    .GlobalEnv$MatrixCacheIndex <- .GlobalEnv$MatrixCacheIndex + 1;
  }
  
  if (!exists("MatrixCache", envir = .GlobalEnv)) {
    .GlobalEnv$MatrixCache <- list();
  }
  myinstance <- .GlobalEnv$MatrixCacheIndex;
  .GlobalEnv$MatrixCache[[myinstance]] <- list( "matrix" = cachedMatrix, "inverse" = NULL);

  
  setMatrix <- function(newMatrix) {
    i <- get("myinstance", envir = parent.env(environment()));
    if (!is.null(.GlobalEnv$MatrixCache[[i]]$matrix)  
        &&  !identical(.GlobalEnv$MatrixCache[[i]]$matrix,newMatrix)) {
      .GlobalEnv$MatrixCache[[i]]$inverse <- NULL;
    }
    .GlobalEnv$MatrixCache[[i]]$matrix <- newMatrix;
    
  }
  getMatrix <- function() {
    i <- get("myinstance", envir = parent.env(environment()));
    x <- .GlobalEnv$MatrixCache[[i]]$matrix ;
    return(x);
  }
  setInverse <- function(inverse) {
    i <- get("myinstance", envir = parent.env(environment()));
    .GlobalEnv$MatrixCache[[i]]$inverse <- inverse;
  }
  getInverse <- function() {
    i <- get("myinstance", envir = parent.env(environment()));
    .GlobalEnv$MatrixCache[[i]]$inverse;
  }
  dispose <- function() {
    i <- get("myinstance", envir = parent.env(environment()));
    .GlobalEnv$MatrixCache[[i]] <- NULL; ##list( "matrix" = NULL, "inverse" = NULL);
  }
  
  list(
    setMatrix = setMatrix, 
    getMatrix = getMatrix, 
    setInverse = setInverse,
    getInverse = getInverse,
    dispose = dispose); 
}



#' cacheSolve sister function to makeCacheMatrix()
#'
#' @param matrixCache cache wrapper instance from  makeCacheMatrix
#' @param ... 
#'
#' @return inverse of matrix
#'
#' @examples see cacheSolve.test() for usage example.
#' 
cacheSolve <- function(matrixCache, ...) {
  
  inverse <- matrixCache$getInverse();
  if (!is.null(inverse)) {
    fromCache <<- TRUE;
  }
  else {     # get cached data
    fromCache <<- FALSE;
    data <- matrixCache$getMatrix();
    inverse <- solve(data); 
    matrixCache$setInverse(inverse); ## store inverse in cache
  }
  return(inverse);
}

cacheSolve.test <- function() {
  matrixA <- matrix( c(2, 4, 3, 1), nrow=2, ncol=2, byrow = TRUE);
  matrixA1 <- matrix( c(5, 4, 3, 1), nrow=2, ncol=2, byrow = TRUE);
  matrixB <- matrix( c(2, 4, 3, 1), nrow=2, ncol=2, byrow = TRUE);
  matrixC <- matrix( c(2, 4, 3, 1), nrow=2, ncol=2, byrow = TRUE);
  
  ## test1.  get inverse and get inverse again.
  test1.cache <- makeCacheMatrix(matrixA);
  test1a.inverse <- cacheSolve(test1.cache);
  test1a.pass <- !is.null(test1a.inverse) && identical(test1a.inverse,solve(matrixA)) && fromCache == FALSE;
  test1b.inverse <- cacheSolve(test1.cache);
  test1b.pass <- !is.null(test1b.inverse) && identical(test1b.inverse, solve(matrixA)) && fromCache == TRUE;
 
  if (test1a.pass) {print("Test1a Pass");}else {print("Test1a Fail");}  
  if (test1b.pass) {print("Test1b Pass");}else {print("Test1b Fail");}  
  test1.cache$dispose();  
  
  ## test2.  check that inverse is rrecalculated when matrix changes.
  test2.cache <- makeCacheMatrix(matrixA);
  test2.inverse1 <- cacheSolve(test2.cache);
  test2.cache$setMatrix(matrixA1);
  #assign("a", "new", envir = .GlobalEnv)
  fromCache <<- FALSE;
  test2.inverse2 <- cacheSolve(test2.cache);
  test2.pass <- !is.null(test2.inverse2) &&  !identical(test2.inverse2,test2.inverse1) && fromCache == FALSE;
  if (test2.pass) {print("Test2 Pass");}else {print("Test2 Fail");} 
  test2.cache$dispose();
  
  ## test3.  use multiple instances of cache to 
  ## make sure a single global cache isn't in play.
  ## that is, test that each cache is independent

  test3.cacheA <- makeCacheMatrix(matrixA);
  test3.cacheB <- makeCacheMatrix(matrixB);
  test3.cacheC <- makeCacheMatrix(matrixC);

  ## request A and B inverse so inverse cache is set
  test3.InverseC1 <- cacheSolve(test3.cacheC);  
  test3.InverseB1 <- cacheSolve(test3.cacheB);
  test3.InverseA1 <- cacheSolve(test3.cacheA);
  
  ## get cached inverses of each to make sure each cache is separate
  test3.InverseC2 <- cacheSolve(test3.cacheC); # non cached 
  test3.InverseB2 <- cacheSolve(test3.cacheB); 
  test3.InverseA2 <- cacheSolve(test3.cacheA); 
  
  test3.pass <- 
    !is.null(solve(matrixC)) && 
    !is.null(solve(matrixB)) && 
    !is.null(solve(matrixA)) && 
    identical(test3.InverseC1,solve(matrixC)) && 
    identical(test3.InverseC2,solve(matrixC)) && 
    identical(test3.InverseB1,solve(matrixB)) && 
    identical(test3.InverseB2,solve(matrixB)) && 
    identical(test3.InverseA1,solve(matrixA)) && 
    identical(test3.InverseA2,solve(matrixA)) ;
  if (test3.pass) {print("Test3 Pass");}else {print("Test3 Fail");}    

  test3.cacheA$dispose();
  test3.cacheB$dispose();
  test3.cacheC$dispose();
}
