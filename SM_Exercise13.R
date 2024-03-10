###Exercise 13: Exam Sheet###

#library for power calculations
library(expm)

#set working directory according to own folder structure
setwd("C:/Users/hgaud/OneDrive/Studium/BASIC3_AdvancedResearchTechniques/ADVANCEDRESEARCH_StochasticModelling")

#read in given data
data <- read.csv("data_AoN_learning_errors.csv", sep="", header=F)


#all-or-none learning on errors model with beta=0.02 and gamma=0.5


#state function with guessing correct=1, guessing error=2, learning=3
simulate_model <- function(beta, gamma, t) {
  initial_state_prob <- c(gamma, 1-gamma, 0)
  transition_matrix <- matrix(c(gamma, 1-gamma, 0, (1-beta)*gamma, (1-beta)*(1-gamma), beta, 0, 0, 1), nrow = 3, byrow = TRUE)
  states <- c(0)*t 
  states[1] <- sample(1:3, 1, prob=initial_state_prob) #sample initial state according to initial state probabilities
  for (i in 2:t) { #sample following states according to transition probabilities
    states[i] <- sample(1:3, 1, prob=transition_matrix[states[i-1], ])
  }
  return(states)
}

#response function with error=0, correct=1
simulate_response <- function(states) {
  responses <- c(0)*length(states)
  for (i in 1:length(states)) {
    if (states[i]==2) {
      responses[i] <- 0
    }
    else {
      responses[i] <- 1
    }
  }
  return(responses)
}


#function to calculate empirical learning curve from simulated data
#(mean of wrong responses per time step)
emp_learning_curve <- function(N, t, beta, gamma) {
  resulting_response <- matrix(nrow=N, ncol=t)
  for (i in 1:N) { #perform simulations of state sequence and respective responses
    resulting_response[i,] <- simulate_response(simulate_model(beta, gamma, t)) 
  }
  return(1-colMeans(resulting_response)) #return mean of wrong responses per column 
}

#plot empirical learning curve of data 
t <- seq(1, dim(data)[2])
plot(t, 1-colMeans(data), type="l",ylim=c(0,1),col="blue", main = "Empirical learning curves of\nall-or-none learning on errors model",
     ylab = "Probabiliy of wrong response" )

#simulate 10 empirical learning curves with N=100, t=50 and add to plot
for(i in 1:10) {
  lines(emp_learning_curve(100, 50, 0.02, 0.5), col="green")
}
legend("topright", c("data", "simulation"), col=c("blue","green"), lty=c(1,1))


#time step at which learning state is reached with >99% 

#version 1: by trial and error
pow = function(x, n) Reduce(`%*%`, replicate(n, x, simplify = FALSE)) #function to compute power of a matrix
state_prob1 <- function(beta, gamma, t) { #probability to be in learning state at time t
  #create transition matrix with given parameters
  initial_states <- c(gamma, 1-gamma, 0)
  transition_matrix <- matrix(c(gamma, 1-gamma, 0, (1-beta)*gamma, (1-beta)*(1-gamma), beta, 0, 0, 1), nrow = 3, byrow = TRUE)
  tm_power <- pow(transition_matrix, t-1)
  #probability to be in L: multiply initial states by column L, put to the power of t-1
  prob <- (initial_states[1]*tm_power[1,3] + initial_states[2]*tm_power[2,3] + initial_states[3]*tm_power[3,3])
  return(prob)
}

state_prob1(0.02, 0.5, 461) #n found by trial and error
print("The learning state is reached with >99% probability at this number of time steps: 461")

#version 2: using package expm for power calculation
learning_state_prob <- function(beta, gamma, prob_learning) {
  t <- 1                                                                 
  prob <- 0 
  transition_matrix <- matrix(c(gamma, 1-gamma, 0, (1-beta)*gamma, (1-beta)*(1-gamma), beta, 0, 0, 1), nrow = 3, byrow = TRUE)
  while (prob < prob_learning) {                                                           
    tm_power <- transition_matrix %^% (t-1) #m-step transition matrix according to current time step
    prob <- gamma*tm_power[1,3] + gamma*tm_power[2,3] #current probability to be in learning state according to m-step transition matrix
    t <- t + 1
  }
  return(list(t, prob)) #list to return multiple values
}

print(paste("In time step", learning_state_prob(0.02, 0.5, 0.99)[1], ", the learning state is reached with a probability of", learning_state_prob(0.02, 0.5, 0.99)[2], "."))


#trajectory probability
trajectory_prob <- function(trajectory, beta, gamma) {
  initial_state_prob <- c(gamma, 1-gamma, 0)
  transition_matrix <- matrix(c(gamma, 1-gamma, 0, (1-beta)*gamma, (1-beta)*(1-gamma), beta, 0, 0, 1), nrow = 3, byrow = TRUE)
  prob <- c(0)*length(trajectory)
  if (trajectory[1]==1) { #probability of first state according to given probability of initial states
    prob[1] <- initial_state_prob[1]
  } else if (trajectory[1]==2) {
    prob[1] <- initial_state_prob[2]
  } else if (trajectory[1]==3) {
    prob[1] <- initial_state_prob[3]
  }
  for (i in 2:length(trajectory)) {  #probabilities of following states according to entries in transition matrix
    if (trajectory[i]==1) {
      prob[i] <- transition_matrix[trajectory[i-1], 1]
    } else if (trajectory[i]==2) {
      prob[i] <- transition_matrix[trajectory[i-1], 2]
    } else if (trajectory[i]==3) {
      prob[i] <- transition_matrix[trajectory[i-1], 3]
    }
  }
  return(prod(prob)) #return product of individual probabilities
}

print(paste("Probability to observe given sequence: ", trajectory_prob(c(1,1,1,2,2,2,2,3,3), 0.02, 0.5)))

