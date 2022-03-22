# -------- Sudoku Solver -------------#


# There are 9 squares
# Each square needs to be filled with the numbers 1-9
# There cannot be any double number in
# 1] Any square
# 2] Any Row
# 3] Any Column


# Create sample sudoku

m = 
  matrix(
    
    c(5, 3, 0, 0, 7, 0, 0, 0, 0,
      6, 0, 0, 1, 9, 5, 0, 0, 0,
      0, 9, 8, 0, 0, 0, 0, 6, 0,
      8, 0, 0, 0, 6, 0, 0, 0, 3,
      4, 0, 0, 8, 0, 3, 0, 0, 1, 
      7, 0, 0, 0, 2, 0, 0, 0, 6,
      0, 6, 0, 0, 0, 0, 2, 8, 0,
      0, 0, 0, 4, 1, 9, 0, 0, 5,
      0, 0, 0, 0, 8, 0, 0, 7, 9
    ),
    nrow = 9, ncol = 9
    
  )

print(m)

# Create function that checks if at certain position, a selected number can be placed

check_field <- function(m, r, c, x) {
  
  # Check if the number already exists in the row
  for (i in 1:9) {
    if (m[r, i] == x) {
      return(F)
    }
  }
  
  # Check the column
  for (i in 1:9) {
    if (m[i, c] == x) {
      return(F)
    }
  }
  
  # Check the square
  r_ = floor((r - 1) / 3)
  c_ = floor((c - 1) / 3)
  
  for (i in 1:3) {
    for (j in 1:3) {
      if (m[r_ + i, c_ + i] == x) {
        return(F)
      }
    }
  }
  
  return(T)
  
}

check_field(m, 1, 3, 1)


find_next_empty_cell <- function(m) {
  for (r in 1:9) {
    for (c in 1:9) {
      if (m[r,c] == 0) {
        return(list(r = r, c = c))
      }
    }
  }
  # No unassigned cell left
  return(list(r = -1, c = -1))
}

find_next_empty_cell(m)

# Solve the Sudoku puzzle
# Check all empty positions
# Keyword: Backtracking
sudoku_solve_recursive <- function(m) {
  
  cell = find_next_empty_cell(m)
  r = cell$r
  c = cell$c
  
  # Check if no empty cell remains
  if (r == -1) {
    return(T)
  }
  
  # Find first number that can be put
  for (n in 1:9) {
    if (check_field(m, r, c, n)) {
      m[r, c] <<- n
      if (sudoku_solve_recursive(m)) {
        return(T)
      }
      # Solution cannot be found. So we return
      m[r, c] <<- 0
    }
  }
  # Arrived at a dead end
  return(F)
}

m <<- m

sudoku_solve_recursive(m)


 