##EXERCISE SHEET 5##

# Exercise 6.1: Random trial incremental model
##  Transition Matrix ##
RTI <- function(alpha, t) {
  
# Initialize transition matrix
  transition_matrix <- matrix(0, nrow = t, ncol = t+1)
  
  for (i in 1:t) {
    for (j in 1:t) {
      if (j == i) {
        transition_matrix[i, j] <- 1 - alpha # no learning
      } else if (j == i + 1) {
        transition_matrix[i, j] <- alpha  # learning
      }
    }
  }
  
# Initialize initial state
  current_state <- 1 #Sets the initial state to 1.
  state_sequence <- numeric(t)  #  Initializes a numeric vector of length t to store the state sequence.
  state_sequence[1] <- current_state # Assigns the initial state to the first element of state_indices
  
# Simulate state transitions
    for (i in 2:t) {# loop starts from 2: initial state already set in state_indices
      # Use a if-else operator to determine the next state based on a random number
      state_sequence[i] <- ifelse(runif(1, 0, 1) <= alpha, state_sequence[i - 1] + 1, state_sequence[i - 1])
    } #If a random number is less than or equal to alpha, transform to next state otherwise stay in the last state.
    
    return(state_sequence) #Returns the generated state sequence
}
# example usage
# Simulation with varying α, t=20
# α = 0.3
states_matrix1 <- RTI(0.3, 20)
states_matrix1

# α = 0.9
states_matrix2 <- RTI(0.9, 20)
states_matrix2

## Response matrix##
#response matrix with input: sequence of states, θ, π and output: sequence of responses
RTI_rm <- function(state_sequence, theta, pi) { #create function
  #response matrix for given input
  response_matrix <- matrix(0, length(state_sequence), 2)
  colnames(response_matrix) <- c("C", "E")
  for (i in 1:length(state_sequence)) {
    response_matrix[i,1] <- 1-((theta^(i-1))*pi)
    response_matrix[i,2] <- ((theta^(i-1))*pi)
  }
  print(response_matrix)
  
  #create response sequence for given input
  response_sequence <- c(length(state_sequence))*0 #vector for responses in the length of the state sequence
  for (i in 1:length(state_sequence)) { #iterate through length of state sequence
    if (runif(1,0,1) <= 1-((theta^(i-1))*pi)) { #probability for correct answer
      response_sequence[i] <- 1 #correct response
    } else {
      response_sequence[i] <- 0 #incorrect response
    }
  }
  return(response_sequence)
}


# Simulation with varying θ, alpha=0.3, pi=0.5
# theta = 0.3
states1 <- RTI(0.3, 20)
states1
responses1 <- RTI_rm(states1, 0.3, 0.5)
responses1
# theta = 0.9
states2 <- RTI(0.3, 20)
states2
responses2 <- RTI_rm(states2, 0.9, 0.5)
responses2

 
#plot variations of θ
plot(responses1, type = "o", col="red", pch=20, xlab=" ", ylab=" ", yaxt="n", ylim=c(0,1))
par(new=TRUE)
plot(responses2, type = "o", col="blue", pch=20, xlab="Time Steps", ylab="Response", yaxt="n")
axis(2, at=0:1, las=2) #modify y-axis
legend("bottomright", legend=c("θ=0.3", "θ=0.9"),
       col=c("red", "blue"), pch=20)
title(main="RTI response space with varying θ (α=0.3, π=0.5)")

#observation: For a higher α, there is more variation in the responses and the responses
#stay in the learned state later. When θ is lower, there is less variation and 
#the responses remain early in the correct state, which indicates a faster learning.


#The RTI model is a generalization of the linear and the all-or-none model.
#How do we have to choose the model's parameters to get a 
# - linear model: state switching by α=1, responses by function with parameters θ and π
# - all-or-none model: state switching by α, responses by g=1-π and θ=1
