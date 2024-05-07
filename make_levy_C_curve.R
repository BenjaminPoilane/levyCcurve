########### parameters ########################################################
# number of iterations
M <- 25000 

# parameters for matrices A and B (scale * rotation(+/- theta))
scale <- 1 / sqrt(2)
theta <- pi / 4

# probability of choosing transformation A
prob <- 0.5

# additive term for transformation B (Bx + fix_term)
fix_term <- c(0.5, 0.5)

# Starting point
current_point <- c(1, 1)

col_A <- "red"
col_B <- "black"
col_background <- "grey90"
########### initialization ####################################################
# Make A and B matrices
B <- scale * matrix(c(cos(theta), sin(theta), 
                      -sin(theta), cos(theta)), 
                    nrow = 2)
A <- t(B)

# initialize empty vectors to store points
current_points <- A_points <- B_points <- matrix(nrow = M, ncol = 2)
current_points[1, ] <- current_point

########### loop ##############################################################
for (i in 1:M) {
  A_points[i, ] <- c(A %*% current_point)
  B_points[i, ] <- c((B %*% current_point) + fix_term)
  
  coin <- rbinom(1, 1, prob)
  
  if (coin) {
      current_point <- c(A_points[i, ])
  } else{
      current_point <- c(B_points[i, ])
  }
  
  current_points[i, ] <- c(current_point)
  
}

########### plot ##############################################################
# limits of the plot
xlim <- range(A_points[, 1], B_points[, 1])
ylim <- c(range(A_points[, 2], B_points[, 2]))

# empty plot
plot(NULL, 
     xlim = xlim, 
     ylim = ylim, 
     xlab = "", ylab = "",
     xaxt = "n", 
     yaxt = "n",
     xaxs = "i", yaxs = "i")

# set background color
rect(xlim[1], ylim[1], xlim[2], ylim[2],
     col = col_background)

# plot A points
points(A_points, 
       col = col_A, pch = 16, cex = 0.3)

# plot B points
points(B_points, 
       col = col_B, pch = 16, cex = 0.3)
