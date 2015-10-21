## The main goal of these two functions is to save computing time when we have to
## calculate the inverse of the same matrix several times.
## 
## The first function "makeCacheMatrix" create a new "Matrix" based in the one 
## introduced as an input. This new "Matrix" is taken by the second function 
## "cacheSOlve" to calculate the inverse. If the inverse of the matrix has never
## been calculated, "cacheSolve" calculates it using the solve() function and save
## it in the cache memory, doing that, if we want to get the inverse of the same 
## matrix again using "cacheSolve", this function returned, directly, the value 
## saved in the cache memory saving computing time.
## 

## "makeCacheMatrix" takes an invertible matrix as input and returns a list 
## with 4 arguments, each of these four arguments is a function that has been
## defined in the body of "makeCacheMatrix". These functions will be very 
## useful to save the inverse result in the cache memory and return this value
## when necessary.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  solveInv <- function(inverted) inv <<- inverted
  getInv <- function() inv
  list(set = set, get = get,
       solveInv = solveInv,
       getInv = getInv)

}


## THis function takes as input the list returned by the first function "makeCacheMatrix.R"
## Once taken the input, "cacheSolve" checks if the inverted matrix has been calculated before
## if that is the case, the function just takes the value from the cache memory and returns it.
## If not, takes the original matrix from the list, determines de inverse, insert the value in
## the cache memory and finally returns it to the user.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$solveInv(inv)
  inv
}
