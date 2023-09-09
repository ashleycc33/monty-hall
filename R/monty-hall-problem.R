#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#' Contestant Selects a Door
#' @description
#' 'select_door()' makes a random door selection between
#' 1 and 3.
#' @details
#' In the specified game, there are three doors with three
#' options: goat, goat, or car. This function gives the
#' doors values of 1, 2, and 3 and randomizes one pick
#' of the three doors.
#' @param
#' No arguments are used in this function.
#' @return
#' A number between 1 and 3, indicating which door was picked.
#' @examples
#' select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host Opens Goat Door
#' @description
#' This function sets up the scenario where
#' the host will always open a door with a
#' goat behind it.
#' @details
#' In this scenario, the host will always open a door
#' with a goat behind it. This door can't be the one
#' the contestant already selected. That is, the door
#' needs to not have a car and can't be a current
#' selection.
#' The two if statements go through the two scenarios
#' where the contestant's pick has a car or goat behind it.
#' @param
#' The function takes in a game and door pick.
#' @return
#' This function returns a number between 1 and 3
#' indicating a door with a goat behind it.
#' @examples
#' open_goat_door( this.game, my.initial.pick )
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Change Doors
#' @description
#' This is the case where the constestant
#' is given the option to change their pick.
#' @details
#' The contestant is given the option to stay with
#' their pick or change. The function runs through
#' two scenarios: the contestant stays with their
#' selection (stay) or not (!stay). If they stay,
#' then nothing changes and their original choice
#' remains (a.pick). If they do not stay, then their
#' new choice has to be the door that is not opened
#' (!= opened.door) and not chosen (!=a.pick).
#' @param
#' The function takes in a T/F statement
#' (stay with current selection or not), the door
#' that has already been opened, and the pick that
#' ends up being the final pick.
#' @return
#' The function returns a number between 1 and 3,
#' indicating the door that the contestant ended up
#' picking (could be the same as original or not).
#' @examples
#' change_door( stay=F, opened.door=1, a.pick=3 )
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine If Contestant Has Won
#' @description
#' This function will determine whether or not
#' the contestant wins (car selection)
#' @details
#' In the game, if the contestant picks a
#' door with a goat behind it, they lose.
#' Conversely, if they pick a door with a car
#' behind it, they win. This function uses the
#' previous pick from the change door function.
#' So, this could either be the door the contestant
#' originally picked or the new one. This function
#' simply returns whether or not the contestant
#' won, based off of their door choice.
#' @param
#' The function takes in the final pick from
#' the previous step and the game.
#' @return
#' "WIN" or "LOSE"
#' @examples
#' determine_winner( final.pick=my.final.pick.stay, game=this.game )
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play the Game
#' @description
#' play_game() simulates an actual run of
#' the game and compares the outcomes of
#' each strategy.
#' @details
#' This function combines multiple elements
#' from the previous steps. A game, first pick,
#' and opened door are created. Two scenarios are
#' created: the contestant either stayed or switched
#' their original door pick. The outcomes of each
#' scenario are created. The strategies are named,
#' outcomes are defined, and the function returns
#' which strategy yielded which result (WIN/LOSE).
#' @param
#' No arguments are used in this function.
#' @return
#' The Win/Lose outcomes of each strategy.
#' @examples
#' play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Play 100 Games (with Outcomes)
#' @description
#' play_n_games is a simulation that plays
#' n games (in this case, n = 100), and reports
#' the proportion of wins/losses.
#' @details
#' Here, we get to see the game in action, multiple
#' times. The loop goes through the set number of games,
#' and then results.df returns the proportion of wins and
#' losses within those 100 plays.
#' @param
#' This function takes in "n": number of games
#' @return
#' A percentage of wins/losses within the
#' 100 plays.
#' @examples
#' play_n_games(n=100)
#' results.df
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}
