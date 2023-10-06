# Setting K
K <- 2

# Setting the seed
set.seed(1)

# Creating the matrix
D <- matrix(rnorm(50*2, mean = 1, sd = 5), 50, 2)
E <- matrix(rnorm(50*2, mean = 10, sd = 5), 50, 2)

F <- rbind(D, E)

# Picks 2 random points from F
C <- F[sample(nrow(F), K, replace = FALSE), ]

# Create a new plot 
plot(c(-15, 30), c(-15, 30), type = "n", xlab = "x", ylab = "y")

points(F, col = "pink")

# Computes the euclidean distance between two points
dist <- function(v1, v2){
  return(sqrt(sum((v1 - v2)^2)))
}

# Creates a function that assigns x_i to D_k=k'
assign <- function(f, c){
  # Loops over each observation x_i in F
  for (i in 1:nrow(f)){
    # Creates empty vectors to be updated
    list_dis <- c()
    k_prime <- c()
    
    # Loops over each k in K
    for (k in 1:K){ 
      
      # Computes the distance d_i_k
      d_i_k <- dist(f[i,], c[k,]) 
      
      # Creates a list of all the distances d_i_k
      list_dis <- c(list_dis, d_i_k) 
    } 
    
    # Finds the arg min k of d_i_k
    min_d_i_k <- which.min(list_dis)
    
    # Assign x_i to the subset k_prime
    F_2[i, ] <- min_d_i_k
  }
  
  return(F_2)
}

# Creates a function that updates the centers
update_centers <- function(c, f, f_2){
  
  # Loops through each element in C
  for(k in 1:K){
    
    # Average of all observations in subset D_k
    c[k, ] <- colMeans(f[f_2 == k, ])
    
    #c[k,] <- (1/ncol(f_2)) * sum(f_2[,k])
    #c[,k] <- colMeans(,  f[f_2[k,]   )
    #c[,k] <- colMeans(f[f_2[k, ], ])
    #c[k,] <- (1/length(f[k]) * sum(f[,k]) )
  }
}

# A function that visualizes c_1, c_2, D_1, D_2
visualize <- function(c, f, f_2){
  
  # Visualizes c_1 and c_2
  points(c[, 1], col = "red")
  points(c[, 2], col = "orange")
  
  # Visualizes D_1
  points(f[f_2 == 1, ], col = "blue")
  
  # Visualizes D_2
  points(f[f_2 == 2, ], col = "green")
}

# Initialize F_2
F_2 <- matrix(nrow = nrow(F), ncol = 1)
#F_2 <- matrix(nrow = nrow(F), ncol = K)


# Initialize F_2_old used to store the previous set of assignments
F_2_old <- c()

#while (TRUE) {
  
  # creates the new subset based of the assignments
  F_2 <- assign(F, C)
  
  # updates the centres
  update_centers(C, F, F_2)
  
  # visualizes the code
  visualize(C, F, F_2)
  
  # Checks whether the new assignments are the same as the previous ones
  if (all(F_2 == F_2_old)){
    
    # if they are the same the iteration stops
    break
  }
  
  #updates the old assignments with the new ones
  F_2_old <- F_2
#}


while(TRUE){
 if(all(F_2 != F_2_old)){
   
  # creates the new subset based of the assignments
  F_2 <- assign(F, C)
  
  # updates the centers
  update_centers(C, F, F_2)
  
  # visualizes the code
  visualize(C, F, F_2)
  
  #updates the old assignments with the new ones
  F_2_old <- F_2
  
}else{
    break
  }
}

#REFERENCES:

#https://stackoverflow.com/questions/7806702/how-do-i-select-a-sample-of-rows-at-random-with-repetition-from-a-matrix-in-r

#I asked chatgpt the following:

#"I have implemented the k-means algorithm in R, but the centers aren't updating. How can I fix this function?"

#What it gave me is detailed in the below:
#Results
#There are several reasons why the centers might not be updating in your k-means implementation. Here are a few things to check:
  
#Check the initialization: One common mistake is to initialize the centers randomly, which can sometimes lead to bad initial centroids. Make sure you are initializing the centroids in a way that makes sense for your data. For example, you could use the k-means++ algorithm to select initial centroids that are well-spaced and representative of the data.

#Check the distance metric: Make sure you are using the correct distance metric to calculate the distance between each point and each centroid. The most common distance metric used in k-means is Euclidean distance, but other metrics like Manhattan distance or cosine similarity may be more appropriate depending on your data.

#Check the convergence criteria: K-means is an iterative algorithm that updates the centroids until they converge. Make sure you have set a convergence criteria that is appropriate for your data. A common criterion is to stop iterating when the centroids stop moving by more than a certain threshold.

#Check the code: Double-check your implementation of the algorithm to make sure there are no bugs or typos that might be causing the centroids to not update. You can try printing out the centroid values at each iteration to see if they are changing.

#Here is an example implementation of k-means in R:

#k_means <- function(data, k, max_iterations) {
  # Initialize centroids
  #centroids <- data[sample(nrow(data), k), ]
  
  #for (i in 1:max_iterations) {
    # Assign each point to nearest centroid
    #distances <- as.matrix(dist(rbind(data, centroids)))[1:nrow(data), (ncol(data)+1):(ncol(data)+k)]
    #cluster <- apply(distances, 1, which.min)
    
    # Update centroids
    #for (j in 1:k) {
      #centroids[j,] <- colMeans(data[cluster == j,])
    #}
  #}
  
  # Return final centroids and cluster assignments
  #return(list("centroids" = centroids, "cluster" = cluster))
#}
#This implementation randomly initializes the centroids, uses Euclidean distance, iterates a maximum number of times, and updates the centroids by taking the mean of the points assigned to each cluster. You can modify this code to fit your specific needs.

#}

#https://www.geeksforgeeks.org/return-the-index-of-the-first-minimum-value-of-a-numeric-vector-in-r-programming-which-min-function/
#https://sparkbyexamples.com/r-programming/concatenate-vector-in-r/#:~:text=To%20concatenate%20a%20vector%20or,combined%20elements%20from%20all%20vectors.

#https://www.tutorialspoint.com/python-check-if-all-elements-in-a-list-are-same
#https://www.oreilly.com/library/view/the-art-of/9781593273842/ch02s05.html
#https://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-numeric-vector



