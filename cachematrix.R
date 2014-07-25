## These functions will get the inverse of a 
## matrix, and if the inverse has been computed
## before, it will be retrieved and not re-computed

##This function makes a list of 4 functions that
##set and get both the matrix and its inverse


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setmatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getmatrix <- function() x
  setmatrixinverse <- function(inverse) i <<- inverse
  getmatrixinverse <- function() i
  list(setmatrix = setmatrix, getmatrix = getmatrix, setmatrixinverse = setmatrixinverse, getmatrixinverse = getmatrixinverse)
}


##This function returns the inverse of a matrix,
##and will get the data from the function above
##if the data is the same as previously

cacheSolve <- function(x, ...) {
  #print(1)
  #return(x)
  m <- x$getmatrixinverse()
  #print(2)
  #return(m)
  if(!is.null(m)) {
    #print(3)
    message("getting cached data")
    #print(4)
    return(m)
  }
  #print(5)
  matrix <- x$getmatrix()
  #print(6)
  i <- solve(matrix, ...)
  #print(7)
  x$setmatrixinverse(i)
  #print(8)
  i
}
