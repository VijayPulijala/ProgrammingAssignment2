## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly (there are also alternatives to 
## matrix inversion that we will not discuss here). 

## the below pair of function are used to create a special object that stores a matrix and   
## cache the inverse of a matrix

## The below function created a special object -- matrix is created that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  inver <- NULL
  set <- function(i){
  x <<- i  
  inver <<- NULL  
  }
  
  get <- function() x
  
 setInver <- function(inverse) inver <<- inverse
 getInver <- function() inver
 list(set = set, get = get,
      setInver = setInver, 
      getInver = getInver)
  
}


## The below function does the inverse of the special object - matrix which is created in the 
## top function (makeCacheMatrix). If inverse is already calculated and the matrix is not reversed 
## then it would retrive the inverse of the matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inver <- x$getInver()
  if(!is.null(inver)) {
    ## matrix is cached already
      return (inver)
  }
  
  res <- x$get()
  inver <- solve(res, ...)
  x$setInver(inver)
  inver
  
}

