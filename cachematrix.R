## Define an invertible matrix
## return: a list containing functions that define and retrieve the matrix and define and retrieve the inverse 
## 



makeCacheMatrix <- function(x = matrix()) {


      inv <- NULL
      define <- function(y) {
        
        # assign a value to an object outside environment 
        
        x <<- y
        inv <<- NULL
      }

      retrieve <- function() x
      
      define_inv <- function(inverse) inv <<- inverse 
      
      retrieve_inv <- function() inv
      
      list(define=define, retrieve=retrieve, define_inv=define_inv, retrieve_inv=retrieve_inv)

      
}





## retrieve output of makeCacheMatrix()
## return: either cached value of inverse of the original matrix or compute inverse 

cacheSolve <- function(x, ...) {

          ## Return a matrix that is the inverse of 'x'

  
  
  inv <- x$retrieve_inv()
  
  # if already calculated then retrieve  
  if (!is.null(inv)){
  
    message("Retrieving Inverse from Cache")
    
    return(inv)
  
}

  # if not then calculate the inverse 
  data <- x$retrieve()
  
  inv <- solve(data, ...)
  
  x$define_inv(inv)
  
  return(inv)

  
  }