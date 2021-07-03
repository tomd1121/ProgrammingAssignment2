#set value of matrix
#get value of matrix
#set value of inverse, using Solve instead of mean as in example provided
#get value of inverse from solve


##returns the inverse of an invertible matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
          set <- function(y){
            x <<- y
            m <<- NULL #set value of matrix
          }
          get <- function() x #get value of matrix
          setSolve <- function(solve) m <<- solve
          #set value of inverse, using Solve instead of mean as in example provided
          getSolve <- function() m #get value of inverse from solve
          list(set = set, 
               get = get, 
               setSolve = setSolve,
               getSolve = getSolve)
}

cacheSolve <- function(x, ...) {
         m <- x$getSolve()
          if(!is.null(m)){
            message("getting cached data")
            return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setSolve(m)
          m
        ## Return a matrix that is the inverse of 'x'
}
