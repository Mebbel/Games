# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Pool Triangles
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Source: "The colossal book of short puzzles and problems" by Martin Gardener 

# Description:
# A set of 15 balls with face values of 1 to 15 is arranged in a flipped triangle
# Find the unique arrangement for which each number below a pair of balls
# is the positive difference of that pair.

# Simple case ------------------------------------------------------------------

# Calculate total number of balls depending on length of first row.
n_1 = 5
n_total = sum(1:n_1)

# Matrix of possible combinations for first row
m_1 = expand.grid(rep(list(1:n_total), n_1))
m_1 = as.matrix(m_1)

# Drop rows with duplicates
m_1 = m_1[apply(m_1, 1, function(x) length(unique(x))) == n_1,]

# Drop rows without the largest number
m_1 = m_1[apply(m_1, 1, max) == n_total,]

# Try all combinations; break when solution found
for (r in 1:nrow(m_1)) {
  
  # Initialize matrix
  m <- matrix(0, nrow = n_1, ncol = n_1)
  
  # Fill first row of matrix
  m[1,] <- m_1[r,]
  
  # Calculate all subsequent rows
  for (i in 2:n_1) {
    for (j in i:n_1) {
      m[i,j] <- abs(m[i-1, j-1] - m[i-1, j])
    }
  }
  
  # Extract upper triangular matrix
  m_u = m[upper.tri(m, diag = T)]
  
  # Check if there are any 0
  if (sum(m_u == 0) == 0) {
    if (length(unique(m_u)) == length(m_u)) {
      break
    }
    
  } 
  
}
  
print(m)
  

# Efficient case ---------------------------------------------------------------

time_start = Sys.time()

# Calculate total number of balls depending on length of first row.
n_1 = 6
n_total = sum(1:n_1)

# Remove duplicates on efficient subsets:
# The largest number n_total needs to be in the first row
# There are n_1 possible positions which define the subsets
m_1 <- 
  lapply(1:n_1, function(i) {
    
    # Create matrix for current position of n_total
    l_i = rep(list(1:(n_total - 1)), n_1)
    l_i[i] = n_total
    m_i = expand.grid(l_i)
    m_i = as.matrix(m_i)
    
    # Drop duplicates
    cols <- combn(1:n_1, 2)
    
    for (c in 1:ncol(cols)) {
      c_1 = cols[,c][1]
      c_2 = cols[,c][2]
      m_i = m_i[m_i[,c_1] != m_i[,c_2],]
    }
    
    m_i
    
  })

m_1 = do.call("rbind", m_1)

l_solutions <- list()

# Try all combinations; break when solution found
for (r in 1:nrow(m_1)) {
  
  # Initialize matrix
  m <- matrix(0, nrow = n_1, ncol = n_1)
  
  # Fill first row of matrix
  m[1,] <- m_1[r,]
  
  # Calculate all subsequent rows
  for (i in 2:n_1) {
    for (j in i:n_1) {
      m[i,j] <- abs(m[i-1, j-1] - m[i-1, j])
    }
  }
  
  # Extract upper triangular matrix
  m_u = m[upper.tri(m, diag = T)]
  
  # Check if there are any 0
  if (sum(m_u == 0) == 0) {
    if (length(unique(m_u)) == length(m_u)) {
      l_solutions[[length(l_solutions) + 1]] <- m
    }
    
  } 
  
}

print(length(l_solutions))
print(Sys.time() - time_start)





