## Takes a matrix as input and returns a list of functions to helper functions to
## 1. Set the value of matrix
## 2. Get the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x <<- y
    inv <<- NULL
  }
  get<-function() x
  setInv <- function(inverse) inv<<-inverse
  getInv <- function() inv
  list(set=set, get=get,
       setInv=setInv, getInv=getInv)

}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv<-x$getInv()
  if(!is.null(inv)){
    inv
  } else {
    data<-x$get()
    inv_val<-solve(data)
    x$setInv(inv_val)
    inv_val
  }
}
