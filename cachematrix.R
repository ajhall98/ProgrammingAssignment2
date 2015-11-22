# =====================================================
# cachematrix.R
# Author: AJ Hall
# Create date:  11/22/15
#
#-------------------------------------
# makeCacheMatrix ()
#
# you are setting up a cacheMatrix
# takes a normal matrix as input# Calling Instructions:
# -----------------
# Run this first to setup a cached matrix
#
# call example >>      mycMatrix1 <- makeCacheMatrix(myNormalMatrix)
#


makeCacheMatrix <- function(matrixPassedIn = matrix()) {  # This function is called only ONCE


     my_cached_matrix <- NULL                  # this initializes the cache

     set <- function(matrixToInvert) {
          matrixPassedIn <<- matrixToInvert
          my_cached_matrix <<- NULL
     }

     get <- function() matrixPassedIn                                   # these are functions
     invertMatrix <- function(invMatx) my_cached_matrix <<- invMatx     # we will pass back
     getInvertedMatrix <- function() my_cached_matrix                   # they are used by the other
     # function
     list(set = set, get = get,
          invertMatrix = invertMatrix,
          getInvertedMatrix = getInvertedMatrix)
}






#-------------------------------------
# cacheSolve ()
#
# input parameter to this function is the cached Matrix instantiated with the above function
#
# call example >>       cacheSolve(mycMatrix1)
# -----------------

cacheSolve <- function(matrixPasssedIn, ...) {

     myInvertedMatrix <- matrixPasssedIn$getInvertedMatrix()   # see if there is a cached version

     if(!is.null(myInvertedMatrix)) {                    # if there is a value in the cache
          message("getting inverted, cached matrix")     # then print the msg, return it and
          return(myInvertedMatrix)                       # jump out with the answer
     }

     # -------------------------------------------
     # if no cached version, then do the stuff below

     data <- matrixPasssedIn$get()           # pass the Vector entered to internal "data" vector

     myInvertedMatrix <- solve(data, ...)    # compute the mean first time

     matrixPasssedIn$invertMatrix(myInvertedMatrix)           # call the invertMatrix function
     # basically just passses out what we
     # just computed
     myInvertedMatrix
}

#--------------------
# eof cachematrix.R
