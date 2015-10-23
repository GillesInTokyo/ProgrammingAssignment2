#Author Gilles Dqn - study program 2015 R Programming Language class
#code validated on a couple of examples 2015/10/21

#--------------------------------------------------------------------------
#Function name: makeCacheMatrix 
#purpose      : caches inverse of matrix and the matrix itself
#arguments    : invertible matrix (matrix assumed invertible)
#return       : list of accesses and settings of features of "special" matrix
#--------------------------------------------------------------------------
makeCacheMatrix <- function (X) {

		#initialize data
		inverseX <- NULL

		#set matrix X in "special" matrix and initialize inverse Matrix
		setMatrix <- function (Y) { 
			X <<- Y 
			inverseX <<-NULL
		}

		#get inital matrix X from "special" matrix
		getMatrix <- function() X

		#set special matrix as matrix X
		setInverse <- function(X) {
			inverseX <<- solve(X)
			return (inverseX)
						  }


		#get inverse matrix from "special" matrix
		getInverse <- function() inverseX   


		#build and return "special" matrix (list)
		list(setMatrix  = setMatrix, 
		     setInverse = setInverse, 
                 getMatrix  = getMatrix, 
                 getInverse = getInverse) 	
}



#--------------------------------------------------------------------------
#Function name: cacheSolve 
#purpose      : caches or retrieve the inverse of matrix
#arguments    : "special" matrix
#return       : inverse of matrix
#--------------------------------------------------------------------------
cacheSolve  <- function (Xspc) {

		#check whether there is an inverse matrix
		inverseX <- Xspc$getInverse()

		if (!is.null(inverseX)) {
			message("getting the inverse matrix")
                  return(inverseX)
                                   }

		#In case did not return compute, set and return inverse matrix
		tempX <- Xspc$getMatrix()		
		Xspc$setInverse(tempX)
            tempXInverse <- Xspc$getInverse()
		
		return(tempXInverse) 

}
