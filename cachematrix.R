## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The function initiates with a variable inv as NULL within the makeCacheMatrix environment
#The variable x is initiated as an empty matrix with no rows or dimensions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #The set function takes an argument named y, this argument y is  assigned the value x from the parent environment,
  #which is the makeCacheMatrix function.
  #When set is executed, it does two things:
  #1. It assigns the object x from parent environment to the input argument.
  #2. It assigns a NULL value to the object inv. This line clears the cache if any functionality was done on the makeCacheMatrix object.
  set<-function(y){
    x <<- y
    inv <<- NULL
  }
  #The get function retrieves the value of x from the parent environment
  #The setinv function solves the inverse of the matrix in the code. The value inv is defined in the parent environment.
  #We access it after setinv() is completed.
  #The makecachematrix function defines the getter for the inverse as inv. A list of all four of these elements.
  #When the function end, it returns a fully formed object of type makeCacheMatrix.
  get <- function()x
  setinv <- function(solve)inv <<- solve
  getinv <- function()inv
  list(set = set, get = get, getinv = getinv, setinv = setinv)
}


## Write a short comment describing this function
#Cachesolve contains a single argument x alongwith additional arguments that you can pass
#The function then tries to retrieve the inverse value from the matrix in the argument.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  #Here the code checks if the value is null, if it is null then the value is retrieved using the get function and the inverse is returned.
  #If value is not null, it returns the cached data using the setinv() function on the input object to set the inverse.  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}


#Test Sample
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

myMatrix_object <- makeCacheMatrix(m1)

cacheSolve(myMatrix_object)

cacheSolve(myMatrix_object)


