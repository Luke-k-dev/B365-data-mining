#HELPER FUNCTIONS
#===================
calc95PercentInterval<- function(phat, n){
    temp = sqrt(phat * (1 - phat))
	zValue = 1.96 *(temp / sqrt(n))
	cat("\n the probability is ", phat,"\n")
	cat("\n\n The 95% interval is from ", phat-zValue, "to", phat+zValue)
	cat("\n the half width of the interval is ", zValue,"\n")
}




#problem 1
#============
problemOne <- function(){
	experamentCount = 10000
	numerOfTimesAWins = 0
	for (i in 1:experamentCount){
		shouldLoop = TRUE
		#simulate the exparement
		while(shouldLoop){
			playerAvalue = isTRUE(runif(1) <= 0.5)
			playerBvalue = isTRUE(runif(1) <= 0.5)
			playerCvalue = isTRUE(runif(1) <= 0.5)
			if(playerAvalue == playerBvalue){
				if(playerBvalue == playerCvalue){
					shouldLoop = TRUE
					next
				}
			
			}
			shouldLoop = FALSE
			if(playerBvalue == playerCvalue){
				numerOfTimesAWins = numerOfTimesAWins + 1
			}
		}
	}
	cat("Player A won", numerOfTimesAWins, "out of ", experamentCount, "times")
	calc95PercentInterval(numerOfTimesAWins/experamentCount, experamentCount)
}

#problemOne() #returns Player a won 3255 out of  10000 times


#problem 2
#=============
problemTwo <- function(exparamentCount){
	plaerAwinCount = 0
	playerAup = TRUE
	for(i in 1:exparamentCount){
		while(TRUE){
			isHeart = isTRUE((runif(1) * 4) <= 1)
			if(playerAup && isHeart){
				plaerAwinCount = plaerAwinCount + 1
			}
			playerAup = !playerAup
			if(isHeart){
				break
			}
			
		}
	}

	cat("Player A won", plaerAwinCount, "out of ", exparamentCount, "times")
	calc95PercentInterval(plaerAwinCount/exparamentCount, exparamentCount)
}

#problemTwo(39000)



#problem 4
#===========

problemFour <- function(exparamentCount){
	countOfA = 0
	for(i in 1:exparamentCount){
		if(isTRUE(runif(1)*10 < 1)){
			countOfA = countOfA + 1
		}
	}
	cat("A occoured", countOfA, "out of ", exparamentCount, "times")
	calc95PercentInterval(countOfA/exparamentCount, exparamentCount)
}

problemFourB <- function(){
	exparamentCount = 300
	countOfA = 0
	upperlist <- c()
	lowerlist <- c()
	for(i in 1:exparamentCount){
		if(isTRUE(runif(1)*10 < 1)){
			countOfA = countOfA + 1
		}
		n = i
		phat = countOfA/n
		temp = sqrt(phat * (1 - phat))
		zValue = 1.96 * (temp / sqrt(n))

		upperlist <- c(upperlist, phat+zValue)
		lowerlist <- c(lowerlist, phat-zValue)

	}
	plot(upperlist, col="red")
	points(lowerlist, col="blue")

	cat("A occoured", countOfA, "out of ", exparamentCount, "times")
	calc95PercentInterval(countOfA/exparamentCount, exparamentCount)
}

problemFourC <- function(){
	inIntervalCount = 0
	for(i in 1:1000){
		exparamentCount = 300
		countOfA = 0
		for(i in 1:exparamentCount){
			if(isTRUE(runif(1)*10 < 1)){
				countOfA = countOfA + 1
			}
		}
		n = exparamentCount
		phat = countOfA/n
		temp = sqrt(phat * (1 - phat))
		zValue = 1.96 * (temp / sqrt(n))
		if(phat - zValue < .1 && phat + zValue > .1){
			inIntervalCount = inIntervalCount + 1
		}
	}

	cat("The acutal probabilty was in the interval ", (inIntervalCount/1000), "% of the time.")
	
}

#problemFour(300)
#problemFourB()
#problemFourC()

#problem five
#=============
problemFive <- function(n){
	runCount = 100000
	greaterCt = 0
	for(i in 1:runCount){
		deck <-c(1:n)
		nums = sample(deck,2, replace = FALSE)
		if(nums[[1]] > nums[[2]]){
			greaterCt = greaterCt + 1
		}
	}

	cat("The probability is ", (greaterCt/runCount),"\n")
}
#see the change for different sized n
#for(i in 2:100){
#	problemFive(i)
#}

#problem 6
#===========
problemSix <- function(){
	p = c(.1,.2,.3,.35, .02, .03)
	num = runif(1)
	cat("num is", num)
	cnum = 0
	counter = 0
	while(cnum <= num){
		counter = counter + 1
		cnum = cnum + p[[counter]]
	}

	cat("Selected event number", counter)
}

problemSix()







