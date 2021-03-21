## This assignment is about solving the inverse of a matrix by caching the 
# result within a lexical scope of a function:  "makeCacheMatrix" and 
# "cacheSolve". Caching is about using memory to avoid excess computation.
# Lexical scopes, allow to create functions within a function and new 
# "user defined" objects (data types) to store data within several environments
###################################

###################################
## The function "makeCacheMatrix" creates a new, unique environment. 
# The inverse matrix is cached inside the object m, within the main 
# environment, which is unique for EACH instance the function is called.
## The output of the function is a list with 5 named elements, which are 
# the five functions defined herein: set, get, setinverse, 
# getinverse and getenv
###################################
## Function makeCacheMatrix
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  z <- NULL  # assigns NULL to a variable within the current environment 
  set <- function(y){ # Set matrix value
    x <<- y # cache the matrix - assigns value y from parent environment
    z <<- NULL # search through parent environments for an existing definition of the variable and set to NULL
  }
  get <- function() x  # Get the matrix value cached with set
  
  setinverse <- function(solve) z <<- solve # Cached value of inverse matrix is saved in z
  getinverse <- function() z # Get the saved value of inverse matrix z that was saved with setinverse
  list(set = set, get = get, # creates list for the four functions  
       setinverse = setinverse,
       getinverse = getinverse)
}


###################################
## The function "cacheSolve" returns the inverse of the matrix that is 
# returned by makeCacheMatrix function, e.g. xMat$getmatrix()
###################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  z <- x$getinverse() # if an inverse has already been calculated this gets it
  if(!is.null(z)) { # check to see if cacheSolve has been run before
    message("getting cached data") 
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z
}
