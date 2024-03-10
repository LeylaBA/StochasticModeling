###EXERCISE SHEET 8###

###Exercise 8.1: m-step transition probabilities###

#plot probabilities to stay in G (y-axis) against α (x-axis) for m=1, m=2 and m=3
alpha_values <- seq(0, 1, by = 0.01)
prob_m1 <- (1-alpha_values)
prob_m2 <- (1-alpha_values)^2
prob_m3 <- (1-alpha_values)^3

plot(alpha_values, prob_m1, type = "l", col = "violet", xlab = "α", ylab = "Probability to stay in G", 
     main = "Simulation of Two-Stage Model")
lines(alpha_values, prob_m2, col = "orange")
lines(alpha_values, prob_m3, col = "darkblue")
legend("topright", legend = c("m=1", "m=2", "m=3"), col = c("violet", "orange", "darkblue"), lty = 1)
