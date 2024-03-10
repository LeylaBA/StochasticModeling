###Exercise Sheet 3###

###Exercise 3.1: Hidden Markov Model###

#state space: S={sun, rain}
states <- c("sun", "rain")

#initial state: [P(X1=sun), P(X2=rain)] = ? (since there is no initial state given, we assume both probabilities to be 0.5)

#transition matrix
transition_matrix <- matrix(data = c(0.7, 0.3, 0.4, 0.6), byrow = TRUE, nrow = 2, dimnames = list(c("sun", "rain"), c("sun", "rain")))
transition_matrix

#response matrix
response_matrix <- matrix(data = c(0.1, 0.9, 0.9, 0.1), byrow = TRUE, nrow = 2, dimnames = list(c("sun", "rain"), c("wet", "dry")))
response_matrix

#function that generates a sequence of a number of given states


weather_sequence <- function(number_states) {
  
  weather_states <- c() #empty vector for sequence of states
  
  for (i in 1:number_states) {
    num <- runif(1,0,1) #random number between 0 and 1 as probability for transition
    state <- sample(states, 1) #random initial state
    
    if (state == "sun"){ #initial state: sun
      if (num < 0.3) { #transition for probability < 0.3
        state <- "rain" #set state to rain
      }
      
    } else { #initial state: rain
      if (num < 0.4) { #transition for probability < 0.4
        state <- "sun" #set state to sun
      } 
    }
    weather_states <- append(weather_states, state) #add current state to sequence
  }
  return(weather_states)
}

#generate sequence of 20 weather states
example_20_states = weather_sequence(20)
example_20_states


response_sequence <- function(states) {
  
  responses <- c() #empty vector for corresponding responses
  
  for (i in 1:length(states)) {
    
    if (states[i] == "sun") {
      response_probs <- c(response_matrix[1,])
      if (runif(1,0,1) < response_probs[1]){
        responses <- append(responses, "wet")
      }else {
        responses <- append(responses, "dry")
      }
    }else {
      response_probs <- c(response_matrix[2,])
      if (runif(1,0,1) < response_probs[1]){
        responses <- append(responses, "wet")
      }else {
        responses <- append(responses, "dry")
      }
    }
  }
  return(responses)
}

response_sequence(example_20_states)

#Example1: Part-of-Speech Tagging in Natural Language Processing, it can predict the part-of-speech tags of words in un-annotated text.
#Example2: it can help traders and investors make informed decisions regarding buying, selling, or holding stocks. 
#Example3: Speech Recognition --> Hidden Markov Models are used to model the sequential nature of phonemes in speech, 
#allowing for the recognition of spoken words and phrases through the analysis of acoustic features.
#Example4: Bioinformatics - Gene Prediction: Hidden Markov Models are employed to predict the locations 
#of genes in DNA sequences by modeling the sequential patterns of coding and non-coding regions,
#aiding in the identification of functional elements within genomes.
#Example5: Google --> predicts the next word in a sentence based on the previous entries within Gmail
#Example6: medical applications --> e.g. cancer detection by analyzing certain sequences and determining how dangerous they might be for the patient


