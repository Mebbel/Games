# ~~~~~~~~~ Craps ~~~~~~~~~~~~~~#



# Only totals for the two dice count. The player sthrows the dice and wins at once if the total for the first throw is 7 or 11.
# Loses at once if it is 2,3, 12. Any other throw is called his point.

# If the first throw is a point, the player throws the dice repeatdely until he either wins by throwing his point again or loses by throwing 7. 

# What is the players's chance to win?


roll_dices <- function(n) {
  sample(1:6, n, replace = T)
}

points_game <- function(winning_sum) {
  # First, we roll the dices again
  roll <- roll_dices(2)
  
  # Then we determine win / or loss
  if (sum(roll) == winning_sum) {
    return(1)
  } else if (sum(roll) == 7) {
    return(0)
  } else {
    # We can throw again.
    points_game(winning_sum)
  }
}

set.seed(123456)

result_simu <- 
  lapply(1:10000, function(x){
    # First round
    roll <- roll_dices(2)
    
    result <- 
      if (sum(roll) == 7 | sum(roll) == 11) {
        # Win
        1
      } else if (sum(roll) == 2 | sum(roll) == 3 | sum(roll) == 12) {
        # Lose
        0
      } else {
        # Point
        points_game(sum(roll))
      }
    
    result
    
  })

result_simu <- unlist(result_simu)
mean(result_simu)



# --- simulate n players each playing x rounds

sim_players <- function(n_players = 1000, n_rounds = 100) {
  
  set.seed(123456)
  number_of_winning_rounds <- 
    lapply(1:n_players, function(n){
      result_simu <- 
        lapply(1:n_rounds, function(x){
          # First round
          roll <- roll_dices(2)
          
          result <- 
            if (sum(roll) == 7 | sum(roll) == 11) {
              # Win
              1
            } else if (sum(roll) == 2 | sum(roll) == 3 | sum(roll) == 12) {
              # Lose
              0
            } else {
              # Point
              points_game(sum(roll))
            }
          
          result
          
        })
      
      result_simu <- unlist(result_simu)
      mean(result_simu)
      
    })
  
  unlist(number_of_winning_rounds)
}



hist(unlist(number_of_winning_rounds), breaks = 30)

sim_players(n_players = 1, n_rounds = 10000)

# Assume 1.000 players in a casino.
# Simulate the each player plays: 10, 100, 250, 500 rounds
library(dplyr)
library(tidyr)
library(ggplot2)

df_simu <- data.frame(n = 1:1000)

df_simu <- 
  df_simu %>%
  mutate(
    r_10 = sim_players(n_rounds = 10),
    r_100 = sim_players(n_rounds = 100),
    r_250 = sim_players(n_rounds = 250),
    r_500 = sim_players(n_rounds = 500),
  )

df_simu_long <- df_simu %>% pivot_longer(cols = -c(n))

ggplot(df_simu_long, aes(x = value, fill = name)) + 
  geom_histogram(binwidth = 0.05, position = position_dodge()) + 
  scale_fill_manual(values = 
                      list("r_10" = "#339933", 
                           "r_100" = "#CC9900",
                           "r_250" = "#CC0099",
                           "r_500" = "#000032"
                           ),
                    labels = c("10", "100", "250", "500")) +
  labs(title="1.000 players in a casino playing craps",x="Share of winning rounds", y = "Number of players")+
  guides(fill = guide_legend(title = "# of rounds")) +
  geom_hline(yintercept = 0) +
  theme_classic()


# And now, let's animate this!
library(gganimate)

df_simu <- data.frame(n = 1:1000)

for (n_r in seq(10, 500, 10)) {
  df_simu <- bind_cols(df_simu, sim_players(n_rounds = n_r))
}

df_simu <- df_simu %>% setNames(c("n", paste0("r_", seq(10, 500, 10))))

df_simu_long <- df_simu %>% pivot_longer(cols = -c(n))

options(gganimate.dev_args = list(width = 1000, height = 730))


df_simu_long %>%
  mutate(name = gsub("r_", "", name)) %>%
  mutate(name = as.integer(name)) %>%
  ggplot(aes(x = value)) + 
  geom_histogram(binwidth = 0.05, fill = "#C5C5FF", color = "#000032") +
  xlim(0, 1) +
  labs(
    title = "1.000 players in a casino playing craps",
    subtitle = '# of rounds: {closest_state}', 
    x = "Share of winning rounds", 
    y = "Number of players") +
  geom_hline(yintercept = 0) +
  theme_classic() +
  transition_states(name) +
  ease_aes('linear')


anim_save(
  filename = "C:/Users/phili/Documents/GitHub/Mebbel.github.io/assets/images/craps_example_distribution_anim.gif")












point_throw <- function(win_n, throw_n = 1) {
  roll <- throw_dice(2)
  throw_n = throw_n + 1
  # print(roll)
  if (sum(roll) == win_n) {
    return(c(1, throw_n))
  } else if (sum(roll) == 7) {
    return(c(0, throw_n))
  } else {
    point_throw(win_n, throw_n)
  }
}

set.seed(1234)


n_scenarios_j = 100
n_scenarios_i = 10000

p_r_j <- vector(length = n_scenarios_j)
n_r_j <- matrix(nrow = n_scenarios_i, ncol = n_scenarios_j)

# And simualte that several times too
for (j in 1:n_scenarios_j) {
  
    p_r <- vector(length = n_scenarios_i)
    n_r <- vector(length = n_scenarios_i)
    
    for (i in 1:n_scenarios_i) {
      
      # First round
      dices <- throw_dice(2)
      # print(dices)
      
      result <- 
        if (sum(dices) == 7 | sum(dices) == 11) {
          # WIN!
          p_r[i] <- 1
          n_r[i] <- 1
        } else if (sum(dices) == 2 | sum(dices) == 3 | sum(dices) == 12) {
          # Lose
          p_r[i] <- 0
          n_r[i] <- 1
        } else {
          # Keep roling until lose or win!
          result <- point_throw(sum(dices))
          p_r[i] <- result[1]
          n_r[i] <- result[2]
        }
      
    }
    
    p_r_j[j] <- sum(p_r) / n_scenarios_i
    n_r_j[,j] <- n_r


}


hist(p_r_j)
hist(n_r_j)

# Probability that the game ends after first throw ? 12 / 36
sum(n_r_j == 1) / length(n_r_j)

# Maximum number of throws before win/lose ?
max(n_r_j)








