scopingExample <- function(x=1) {
	y <- 5
	if (exists("n")) {
	 	print("n exists")
		} 
		else {
			print("n does NOT exist")
		}
	#missing means the argument is not provided when the function is called
	if (missing(x)) {      
		print("x is missing")
		l <- paste("x default value is", x, sep=": ")
		print(l)
		} 
		else {
			print("x is NOT missing")
			l <- paste("x provided value is", x, sep=": ")
			print(l)
		}
	z <- paste("value of y is", y, sep=": ")
	print(z)
	rm(y)
	if (exists("y") == TRUE) {
		z <- paste("now the value of y is", y, sep=": ")
		print(z)
	} else {
		print("NO more y")
	}
}