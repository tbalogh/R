rm(list=ls())


# 1 4 7 10
# 2 5 8 11
# 3 6 9 12

R <- -0.04

DIRECTION <- list(NORTH=0,EAST=1,SOUTH=2,WEST=3)

has_wall_north <- function(s) {  return(s %in% c( 1,   4,  7, 10, 6))  }
has_wall_east <- function(s)  {  return(s %in% c(10,  11, 12,  2   ))  }
has_wall_south <- function(s) {  return(s %in% c( 3,   6,  9, 12, 4))  }
has_wall_west <- function(s)  {  return(s %in% c( 1,   2,  3,  8   ))  }

step_will_suceed <- function() {  return(runif(1) > 0.2)  }

roll_norm <- function(dirs) { return(sample(dirs, size = 1)) }

roll_direction <- function(a) {
  if (a == DIRECTION$NORTH || a == DIRECTION$SOUTH) {
    return(roll_norm( c(DIRECTION$WEST, DIRECTION$EAST) ))
  }
  else {
    return(roll_norm( c(DIRECTION$NORTH, DIRECTION$SOUTH) ))
  }
}

step <- function(s, a) {
  dir <- a
  # nem arra megyunk amerre terveztuk
  if (!step_will_suceed()) {
    dir <- roll_direction(a)
  }
  do_step(s, dir)
}

concrete_step <- function(s, can_step, ds) {
  if(can_step(s)) return(list(ns=s, r=R)) else return(list(ns=s + ds, r=R))
}
step_north <- function(s) { return(concrete_step(s, has_wall_north, -1))  }
step_east  <- function(s) { return(concrete_step(s, has_wall_east,  +3))  }
step_south <- function(s) { return(concrete_step(s, has_wall_south, +1))  }
step_west  <- function(s) { return(concrete_step(s, has_wall_west,  -3))  }

do_step <- function(s, a) {
  #mostantol csak a fel lehet akadaly, tehat akkor nem lepunk, minden esetben lepunk
  if (a == DIRECTION$NORTH) {
    return( step_north(s) )
  }
  else  if (a == DIRECTION$EAST) {
    return( step_east(s) )
  }
  else if (a == DIRECTION$SOUTH) {
    return( step_south(s) )
  }
  else {
    return( step_east(s) )
  }
}




# TESTS
#######

TEST_STATES <- 1:12
WITH_NORTH_WALLS <- c( 1,   4,  7, 10, 6)
WITH_EAST_WALLS  <- c(10,  11, 12,  2   )
WITH_SOUTH_WALLS <- c( 3,   6,  9, 12, 4)
WITH_WEST_WALLS  <- c( 1,   2,  3,  8   )

test_step_will_suceed <- function() {
  accumulator <- 0
  trial_count = 100;
  for(i in 1:trial_count) {
    accumulator <- accumulator + step_will_suceed()
  }
  print(accumulator / trial_count)
}

test_roll_norm <- function() {
  dirs_trial <- c(0, 0)
  dirs <- c(1, 2)
  for(i in 1:100) {
    rolled_dir <- roll_norm(dirs)
    dirs_trial[rolled_dir] <- dirs_trial[rolled_dir] + 1
  }
  print(dirs_trial)
}

test_roll_direction <- function() {
  trial_count <- 100
  
  # test north
  dir <- DIRECTION$NORTH
  for (i in 1:trial_count) {
     rolled_dir <- roll_direction(dir)
     if (rolled_dir != DIRECTION$WEST && rolled_dir != DIRECTION$EAST) {
       stop(paste("Wrong direction rolled to north:", toString(rolled_dir)))
     }
  }
  
  # test west
  dir <- DIRECTION$WEST
  trial_count <- 100
  for (i in 1:trial_count) {
    rolled_dir <- roll_direction(dir)
    if (rolled_dir != DIRECTION$NORTH && rolled_dir != DIRECTION$SOUTH) {
      stop(paste("Wrong direction rolled to north:", toString(rolled_dir)))
    }
  }
}

test_step_north <- function() {
  #invalid step
  for(s in WITH_NORTH_WALLS) {
    res <- step_north(s)
    if (res$ns != s || res$r != R) stop("Moved to a NORTH WALL")
  }
  
  #valid step
  for(s in TEST_STATES[-WITH_NORTH_WALLS]) {
    res <- step_north(s)
    if (res$ns != (s-1) || res$r != R) stop("Did not move NORTH")
  }
}

test_step_east <- function() {
  #invalid step
  for(s in WITH_EAST_WALLS) {
    res <- step_east(s)
    if (res$ns != s || res$r != R) stop("Moved to a EAST WALL")
  }
  
  #valid step
  for(s in TEST_STATES[-WITH_EAST_WALLS]) {
    res <- step_east(s)
    if (res$ns != (s+3) || res$r != R) stop("Did not move EAST")
  }
}

test_step_south <- function() {
  #invalid step
  for(s in WITH_SOUTH_WALLS) {
    res <- step_south(s)
    if (res$ns != s || res$r != R) stop("Moved to a SOUTH WALL")
  }
  
  #valid step
  for(s in TEST_STATES[-WITH_SOUTH_WALLS]) {
    res <- step_south(s)
    if (res$ns != (s+1) || res$r != R) stop("Did not move SOUTH")
  }
}

test_step_west <- function() {
  #invalid step
  for(s in WITH_WEST_WALLS) {
    res <- step_west(s)
    if (res$ns != s || res$r != R) stop("Moved to a WEST WALL")
  }
  
  #valid step
  for(s in TEST_STATES[-WITH_WEST_WALLS]) {
    res <- step_west(s)
    if (res$ns != (s-3) || res$r != R) stop("Did not move WEST")
  }
}

test_step_will_suceed()
test_roll_norm()
test_roll_direction()
test_step_north()
test_step_east()
test_step_south()
test_step_west()