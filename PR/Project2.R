# CHIEHYANG CHIEN 131037

# Define nash rules
getAllPureStrategyNE <- function(game) {
  n_players <- length(game)
  
  # Create a list of strategy for each player
  strategies <- lapply(game, function(x) seq_len(dim(x)[1]))
  
  # Generate all combinations of strategies
  strategy_combinations <- expand.grid(strategies)
  
  # Extract payoff for a given strategy and player
  getPayoff <- function(player_game, strategy) {
    # Dynamic indexing for multi-dimensional array
    index <- as.list(strategy)
    return(do.call("[", c(list(player_game), index)))
  }
  
  # Check if a strategy is a Nash Equilibrium
  isNashEquilibrium <- function(strategy) {
    for (player in seq_len(n_players)) {
      current_strategy <- strategy[player]
      # Get the payoff matrix for the player and compare payoffs
      player_game <- game[[player]]
      current_payoff <- getPayoff(player_game, strategy)
      
      for (alt_strategy in strategies[[player]]) {
        if (alt_strategy != current_strategy) {
          # Check the payoff for an alternate strategy
          alt_strategy_vec <- strategy
          alt_strategy_vec[player] <- alt_strategy
          alt_payoff <- getPayoff(player_game, alt_strategy_vec)
          if (alt_payoff > current_payoff) {
            return(FALSE)
          }
        }
      }
    }
    return(TRUE)
  }
  
  # Filter the Nash Equilibriums
  nash_equilibria <- Filter(isNashEquilibrium, split(strategy_combinations, seq_len(nrow(strategy_combinations))))
  return(nash_equilibria)
}

# Example1 (prisoners' dilemma for two players)
game <- list(
  "player1" = array(c(5, 10, 1, 2), dim = c(2, 2)),
  "player2" = array(c(5, 1, 10, 2), dim = c(2, 2))
)
getAllPureStrategyNE(game)

# Example2 (stag-hare game for two players)
game <- list(
  "player1" = array(c(3, 2, 0, 2), dim = c(2, 2)),
  "player2" = array(c(3, 0, 2, 2), dim = c(2, 2))
)
getAllPureStrategyNE(game)

# Example3 (anti-coordination game for  three players)
game <- list(
  "player1" = array(c(0, 1, 1, 1, 1, 1, 1, 0), dim = c(2, 2, 2)),
  "player2" = array(c(0, 1, 1, 1, 1, 1, 1, 0), dim = c(2, 2, 2)),
  "player3" = array(c(0, 1, 1, 1, 1, 1, 1, 0), dim = c(2, 2, 2))
)
getAllPureStrategyNE(game)

# Example4 (No Nash equilibrium)
game <- list(
  "player1" = array(c(0, 2, 0, -2), dim = c(2, 2)),
  "player2" = array(c(0, 0, -2, 6), dim = c(2, 2))
)
getAllPureStrategyNE(game)


