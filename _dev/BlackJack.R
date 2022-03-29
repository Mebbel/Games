# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Black Jack
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library(dplyr)
library(tidyr)

# Define dealer settings
dealer <- 
  list(
    
    
  )

# Define player settings
player <- 
  list(
    
    
  )

# Define numerical values of cards
values <- 
  list(
    
  )


# Helper functions

createCards <- function(type = "french") {
  
  if (type == "french") {
    
    faces <- c(
      1:10, "B", "Q", "K", "A"
    )
    
    colors <- c(
      "Clubs", "Spades", "Hearts", "Diamonds"
    )
    
    # Create set of cards
    set <- expand.grid(faces, colors, stringsAsFactors = F) 
    names(set) <- c("face", "color")
    
  }

  return(set)
  
}


shuffleCards <- function(set, n = 5) {
  
  set.seed(1234)
  
  # Shuffle the deck n times
  for (i in 1:n) {
    set <- set[sample(1:nrow(deck), nrow(deck), replace = F), ]  
  }
  
  return(set)
}

drawCards <- function(set, n = 2) {
  
  # Draw from the top
  cards <- set[1:2,]
  set <- set[-c(1:2),]

  return(
    list(
      cards = cards,
      deck = set
    )
    
  )
    
}


# Start the game ------------------------------------------------------------

# Initialize players
players <- tibble(
  round = 0,
  players = 1:4,
  cash = 100,
  bet = 0,
  cards = list(list(face = "", color = "")),
  value = 0
)

# Create a set of cards
deck <- createCards(type = "french")

# Shuffle the cards
deck <- shuffleCards(deck)

# Begin round ~~~~~~~~~~~~~~~~~~~~~~~

# Place bets in betting boxes
players <- 
  players %>%
  bind_rows(
    players %>% 
      filter(round == max(round)) %>%
      mutate(
        round = round + 1,
        bet = pmin(10, cash),
        cash = cash - bet
      )
  )

# Initial distribution of cards
i = 1

# Draw cards for each player

for (i_p in 1:4) {
  
  draw <- drawCards(deck, n = 2)
  deck <- draw$deck
  cards <- draw$cards  
  
  players[players$round == i & players$players == i_p, "cards"] <- cards
  
}






# Draw card for dealer


# Now, let each player play






