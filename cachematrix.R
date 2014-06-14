## These functions cache the inverse matrix of an invertible matrix.
## If the matrix isn't going to change and the inverse matrix will be needed
## more than once, this can save time by fetching the cached answer and not
## resolving the problem each time.

## This function takes an invertible matrix as input (this is assumed, not checked)
## It then stores four functions that can 1) set (and reset) the cached values in
## an outside environment, 2) get the original matrix, 3) solve for the inverse matrix
## and cache the result, and 4) get the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
      inv.m <- NULL
      set <- function(y) {
            x <<- y
            inv.m <<- NULL
      }
      get <- function() x
      set.inv.matrix <- function(solve) inv.m <<- solve
      get.inv.matrix <- function() inv.m
      list(set = set, get = get,
           set.inv.matrix = set.inv.matrix,
           get.inv.matrix = get.inv.matrix,
           )
}

## cacheSolve takes, as an input, the output from the makeCacheMatrix function
## If you try to use this function with a matrix you will recieve an 'Error in 
## x$get.inv.matrix.

## The function checks to see if the inverse matrix has already been cached. If it has
## it returns the cached inverse matrix. If it hasn't already been cached the function
## calculates the inverse matrix and then 'sets' it in the cache.

cacheSolve <- function(x, ...) {
      inv.m <- x$get.inv.matrix()
      if(!is.null(inv.m)) {
            message("getting cached data")
            return(inv.m)
      }
      data <- x$get()
      inv.m <- solve(data, ...)
      x$set.inv.matrix(inv.m)
      inv.m
}
