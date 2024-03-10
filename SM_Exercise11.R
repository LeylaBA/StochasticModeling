### Exercise Sheet 11 ###

data <- read.csv("C:\\Users\\leyla\\Desktop\\Stochastic Modelling\\Exercise11\\data.csv", sep="", header=F)
typeof(data[1,1])


# Task 1
emp_ebl_perSubj <- function(d){ #d --> data
  EbL_for_t <- vector("list", 50) #A list of 50 containing cumulative number of errors of participants at each time step as its elements
  for (i in 1:50){ #Number of trials  
    EbL <- 0 #A variable that will temporarily hold the number of
    for (j in 1:30){ #Number of participants
      if (d[j,i] == FALSE){
        EbL = EbL + 1
      }
    }
    EbL_for_t[i] <- EbL
  }
  return(EbL_for_t)
}

print(emp_ebl_perSubj(data))
ebl_data = emp_ebl_perSubj(data)


#Task 2
total_errors <- sum(sapply(ebl_data, sum))
mean_ebl = total_errors/30
print(mean_ebl)


#Task 3
g = 0.5
est_alpha = (1-g)/mean_ebl
print(est_alpha)


#Task 4
#Plotting theoretical learning curve of AoN with the estimated alpha value
t<-seq(1,50,1)
plot(t, (1 - g)*(1 - est_alpha)^(t-1), type="l", ylim=c(0,g), col="violet", main = "Empirical and Theoretical Learning Curve of AoN with αˆ = 0.26, g = 0.6",
     ylab = "Probabiliy of wrong response at time t" )

#Plotting empirical learning curve of the data
emp_error <- vector("list", 50)
for (i in 1:50){
  prob = ebl_data[[i]]/30        #A quick for loop to transform the raw number errors into probabilies at each time step
  emp_error[i] <- prob
}

lines(t, emp_error, type = "l", ylim=c(0,g), col="green")











