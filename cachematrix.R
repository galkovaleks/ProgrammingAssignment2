initMatrixCache <- function(x = matrix()) {
  
	# 'initMatrixCache'. Метод создает кеширующую матрицу из созданной 

	myMatrix <- NULL	#инициализация метода 'myMatrix'    

	setMatrix <- function(y) {
		x <<- y
		myMatrix <<- NULL
	}	#определение метода 'setMatrix'  

	getMatrix <- function() x	#определение метода 'getMatrix'. Метод возвращает матрицу 'x'    

	setCache <- function(inverse) myMatrix <<- inverse	#определение метода 'setCache'    

	getCache <- function() myMatrix	#определение метода 'getCache'. Возвращает обратную матрицу 'x'    

	list(setMatrix = setMatrix,
		getMatrix = getMatrix,
		setCache = setCache,
		getCache = getCache)  	#список имен методов доступных извне
}
 #---------------------------------------------------------


inverceMatrixCache <- function(x, ...) {  
  
	# 'inverceMatrixCache'. Возвращает обратную матрицу  

	myMatrix <- x$getCache()    #Проверка полученной матрицы  

	if (!is.null(myMatrix)) {
		message("loading cache matrix...")
		return(myMatrix)
	}	#Обработка не пустой матрицы   
	else {
		dMatrix <- x$getMatrix()
		myMatrix <- solve(dMatrix, ...)
		x$setCache(myMatrix)
		return(myMatrix)
	}  #Обработка случая пустой матрицы
}
#---------------------------------------------------------
