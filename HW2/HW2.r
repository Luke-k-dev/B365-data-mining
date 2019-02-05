#HELPER FUNCTIONS
#===================
calc95PercentInterval<- function(phat, n){
    temp = sqrt(phat * (1 - phat))
	zValue = 1.96 *(temp / sqrt(n))
	cat("\n the probability is ", phat,"\n")
	cat("\n\n The 95% interval is from ", phat-zValue, "to", phat+zValue)
	cat("\n the half width of the interval is ", zValue,"\n")
}


#problem 3
#============
problemThree <- function(){
	expCount = 1000
	numerOfTimesAWins = 0
	outA = c()
	outB = c()

	for (i in 1:expCount){
		nums = runif(2)
		a = FALSE
		b = FALSE
		if (nums[1] + nums[2] < 1){
			a = TRUE
		}
		if(nums[1] - nums[2] < 0){
			b = TRUE
		}
		outA = c(outA, a)
		outB = c(outB, b)
	}


	TT = 0
	TF = 0
	FT = 0
	FF = 0
	A = 0
	B = 0

	for (i in 1:expCount){
		if(isTRUE(outA[i])){
			A = A + 1
		}
		if(isTRUE(outB[i])){
			B = B + 1
		}
		if(isTRUE(outA[i]) && isTRUE(outB[i])){
			TT = TT + 1
		}
		if(isTRUE(outA[i]) && !isTRUE(outB[i])){
			TF = TF + 1
		}
		if(!isTRUE(outA[i]) && isTRUE(outB[i])){
			FT = FT + 1
		}
		if(!isTRUE(outA[i]) && !isTRUE(outB[i])){
			FF = FF + 1
		}
	}
	calc95PercentInterval(A/expCount, expCount)
	calc95PercentInterval(TT/expCount, expCount)
	barplot(c(TT, TF, FT, FF), col="red", names.arg=c("TT", "TF", "FT", "FF"))
}
problemThree()