## The functions makeCacheMatrix() and cacheSolve() work together to 
## optimize access to an inverted matrix.


## Call makeCacheMatrix() with an invertible square matrix. This will
## return a list of functions get and set input matrix and inverted matrix

makeCacheMatrix <- function(mat = matrix()) {
  imat <- matrix()
  set <- function(y) {
    if (!identical (mat,y) ) {
      mat <<- y 
      imat <<- matrix()   }
  }
  get <- function() mat
  setInv <- function(nuInv) imat <<- nuInv
  getInv <- function() imat
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Call cacheSolve() with an object returned by makeCacheMatrix() to obtain
## the inverse of teh matrix passed to makeCacheMatrix(). If the inversion
## had already been done, with an earlier call, then the cached inverse is
## returned saving potentially expeensive/time-consimung computation. If
## the fucntion is being called for the first time using a given matrix then
## it computes the inverse using solve(), returns and caches the result

cacheSolve <- function (mfl, ...) {
  
  cimat <- mfl$getInv()      # get the cached invert
  # Is invert empty? this matrix has not been inverted yet? 
  # If so invert and set the cache.
  if (identical(cimat, matrix())) {  
    cmat <- mfl$get()
    cimat <- solve(cmat)
    mfl$setInv(cimat) }
  else {
    message( "Getting from cache")
  }
  cimat
}

