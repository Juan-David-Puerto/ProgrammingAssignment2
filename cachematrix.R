
#The makeCacheMatrix function creates a special "matrix",
#which is really a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse of the matrix
#4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  invrs <- NULL
  set <- function(y){
    # Use the <<- operator to assign a value to an object in an
    # environment that is different from the current environment
    x <<- y
    # this assigns a value of NULL to the invrs object in the parent environment.
    # Cleaning any value of n1 that had been cached by a prior execution of 
    #cacheSolve()
    invrs <<- NULL
  }
  get <- function() x
  setinverse <- function (inverse) invrs <<- inverse
  getinverse <- function () invrs
  
  list(set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


#The cacheSolve function calculates the inverse of the special "matrix"
#the special "matrix" which created with the makeCacheMatrix function.
#However, it first checks to see if the inverse has already been calculated.
#If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the matrix and sets the value of the
#inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...){
  ## Return a matrix "invrs" that is the inverse of 'x'
  invrs <- x$getinverse()
  ##if the value here is not equal to NULL, we have a valid, cached inverse and
  ##can return it to the parent environment
  if (!is.null(invrs)){
    message("Getting cached data")
    return(invrs)
  }
  mat <- x$get()
  invrs <- solve(mat,...)
  x$setinverse(invrs)
  invrs
}

####################################
#Example the use number 1
#my_matrix <- makeCacheMatrix(matrix(sample(c(1,2,3), size=9, replace=TRUE), 3, 3))
#my_matrix$get()
#my_matrix$getinverse()

#cacheSolve(my_matrix) #calculate the inverse matrix for the first time 
#cacheSolve(my_matrix) #Returns the inverse array from the cache 
#my_matrix$getinverse()

####################################
#Example the use number 2
#my_matrix$set(matrix(sample(c(2,3,4), size=4, replace=TRUE), 2, 2))
#my_matrix$get()
#my_matrix$getinverse()
#cacheSolve(my_matrix)
#cacheSolve(my_matrix)
#my_matrix$getinverse()

