
# Pathfinder - mazes !!!

# Source https://www.youtube.com/watch?v=aKYlikFAV4k&t=78s
# Coding Challenge 51.1: A* Pathfinding Algorithm - Part 1

# Typical problem case: train networks with different cities.
# What is the optimal route?


# A* search algorithm

# More efficient than Dijkstra's algorthm, which just tests all possible paths




# Create 2D Array - playing field, where each field can hold several values
# Work with list of lists


n_cols = 5
n_rows = 5


grid = list()

for (i in 1:n_cols) {
  grid[[i]] = list()
}

for (i in 1:n_cols) {
  for (j in 1:n_rows) {
    grid[[i]][[j]] = list(x = i, y = j, f = 0, g = 0, h = 0)
  }
}

grid[[1]][[3]]
 

# openSet: List of nodes that still need to be evaluated
# closedSet: List of nodes that were already evaluated and will not be revisited

openSet = list()
closedSet = list()

# Define start and end of path
start = grid[[1]][[1]]
end = grid[[n_cols]][[n_rows]]


# Push start to openSet
openSet = list(openSet, start)

if (length(openSet) > 1) {
  
  # TODO: Video at 23:08 !!!
  # https://www.youtube.com/watch?v=aKYlikFAV4k?t=23m9s
  
  lowestIndex = 1
  
  for ()
  
  
} else {
  # No solution found
}



