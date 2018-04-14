## Put comments here that give an overall description of what your
## functions do
##  makeCacheMatrix Initializes an object that will get and set matrices, and get and set the inverse of the set matrix 
##  cacheSolve checks to see if the inverse of the matrix has been calculated and cached.  
##     If it hasn't, it solves and caches it, saves it to the cache and returns it

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    #x initialized in function argument, m initialized below
    m <- NULL
    set <- function(y) {
      #does the same thing as x & m initializations above
      #assigns to parent environment
      x <<- y
      #clears any previously cached value because x has been reset
      m <<- NULL
    }
    #gets x from the parent environment
    get <- function() x
    #creates the new function then assigns the input argument value to m in the parent environment
    setinverse <- function(solve)  m <<- solve  
    #the new function gets the value (from the parent environment)
    getinverse <- function() m
    # returns this list of elements to parent environment, elementName=value allows $ syntax
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Write a short comment describing this function

#requires input matrix of type makeCacheMatrix, not just any matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    #attempts to get the inverse from the passed in matrix
    m <- x$getinverse()
    if(!is.null(m)) {
      #this is a cached inversion for the passed in x as its always reset in makeCacheMatrix
      #its therefore valid, so return it
      message("getting cached data")
      return(m)
    }
    #gets the passed in matrix
    data <- x$get()
    #assign the inverse of the matrix locally 
    m <- solve(data, ...)
    #assign the local value to 
    x$setinverse(m)
    #return the value
    m
}

##testing

#set up 
my_matrix<-makeCacheMatrix(matrix(c(3,4,9,7),ncol=2,nrow=2))
#show the input/current matrix
my_matrix$get()
#show the inverse, should be null, as not solved & cached yet
my_matrix$getinverse()
my_matrix$set(matrix(c(11,2,5,1),ncol=2,nrow=2))          # reset value with a new matrix
cacheSolve(my_matrix)
my_matrix$get()
my_matrix$getinverse()
my_matrix$set(matrix(c(3,4,9,7),ncol=2,nrow=2))          # reset value with a new matrix
my_matrix$get()
#show the inverse, should be null, as not solved & cached yet
my_matrix$getinverse()
cacheSolve(my_matrix)
my_matrix$get()
#should have inverse value now
my_matrix$getinverse()

