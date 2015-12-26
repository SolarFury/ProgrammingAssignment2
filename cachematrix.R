## Programming Assignment number2
## the first function makeCacheMAtrix that stores a matrix and its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  
  setMatrix <- function (x) {
    matrix <<- x
    inverse <<- NULL
  }
  
  ## returnes the matrix strioed in the object
  getMatrix <- function() matrix
  
  ## will solve for the inverse if passed NULL
  ## otherwise just stores what is passed into the inverse object
  setInverse <- function(m) {
    if(is.null(m))
    {
      message("Passed NULL argument")
      if(!is.null(matrix))
      {
        message("Object initialised computing inverse")
        cacheSolve(matrix)
      }
      else
      {
        message("Nothing I can do") 
      }
    }
    else
    {
      inverse <- m
    }
  }
    
  ## returns whatever is stored in the inverse object
  getInverse <- function() {
    inverse
  }

  ##returns a list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## takes a special matirc created using the function above
## returns the inverse, computes and stroes the inverse if it has not been cached already

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Also stores the cached value in a CacheMatric Object
  inverse <- x$getInverse()
  ##checking if inverse is cached
  if(!is.null(inverse))
  {
    return(inverse)
  }
  
  ## solve for the inverse (assumes the matirx is invertable)
  matrix <- x$getMatrix()
  inverse <- solve(matrix)
  x$setInverse(inverse)
  
  ## return the inverse
  inverse
}
