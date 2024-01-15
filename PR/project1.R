#CHIEHYANG CHIEN 131037

install.packages("magick")
library(magick)
# File path
image_path <- "ubuntu.png"
# Read the image
a0 <- image_read(image_path)
# Get relevant part
dims <- rev(dim(as.raster(a0)))
a1 <- image_crop(image = a0, geometry = "800x800+560+140")
#Quantizing
a2 <- as.raster(image_quantize(image_normalize(a1), max = 2))
#Changing to TRUE / FALSE matrix
a3 <- array(
  ifelse(as.vector(a2) == unique(as.vector(a2))[1], TRUE, FALSE),
  dim(a2)
)
# Save image
saveRDS(object = a3, file = "./maze.RDS")
# Display the image
maze <- readRDS(file = "./maze.RDS")
plot(as.raster(maze))

# Define the BFS function
pathQ <- function(maze, start, end) {
  # Define the possible moves (up, down, left, right)
  moves <- rbind(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
  
  # Initialize the queue with the start point and mark it as visited
  queue <- list(start)
  visited <- matrix(FALSE, nrow = nrow(maze), ncol = ncol(maze))
  visited[start[[1]], start[[2]]] <- TRUE
  
  # BFS loop
  while (length(queue) > 0) {
    current <- queue[[1]]  # Get first element in the queue
    queue <- queue[-1]     # Remove it from the queue
    
    # Check the current position is within the end region or not
    if (current[[1]] %in% end[[1]] && current[[2]] %in% end[[2]]) {
      return(TRUE)
    }
    
    # Iterate over all possible moves
    for (move in 1:nrow(moves)) {
      next_x <- current[[1]] + moves[move, 1]
      next_y <- current[[2]] + moves[move, 2]
      
      # Check the next move is within the maze boundaries and an open path or not
      if (next_x >= 1 && next_x <= nrow(maze) &&
          next_y >= 1 && next_y <= ncol(maze) &&
          !visited[next_x, next_y] && maze[next_x, next_y]) {
        # Mark the next move as visited and add it to the queue
        visited[next_x, next_y] <- TRUE
        queue <- c(queue, list(c(next_x, next_y)))
      }
    }
  }
  # If it not reach end region, return FALSE
  return(FALSE)
}

# Starting point and endregion 
startPoint <- c(1, 1) 
endRegion <-list(x = 387:413, y = 322:348)  

# Result for a path
result <- pathQ(maze, startPoint, endRegion)
print(result)


