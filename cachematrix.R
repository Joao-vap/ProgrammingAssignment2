
#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv_matrix <- NULL
  
  #Setting the oject
  set <- function(y){
    x <<- y
    inv_matrix <<- NULL
  }
  
  #Getting the object
  get <- function() x
  
  #Setting the result
  setinv <- function(inverse) inv_matrix <<- inverse
  
  #Getting the result
  getinv <- function() inv_matrix
  
  #Returning a list with functions (named)
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
  
}


#This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
#If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve`
#should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  #Getting value for inverse
  inv_matrix <- x$getinv()

  #Checking if there is already a result to return
  if (!is.null(inv_matrix)) {
    message("getting in cache")
    return(inv_matrix)
    
  } 

  #Othewise, we need to calculate the inverse
  #Get obect
  data <- x$get()
  
  #Inverse it
  inv_matrix <- solve(data)
  
  #Setting in var the result to future comp
  x$setinv(inv_matrix)
  
  #Returning result
  inv_matrix

}

