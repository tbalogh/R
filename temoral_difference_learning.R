rm(list=ls())


# 1 4 7 10
# 2 5 8 11
# 3 6 9 12

R <- -0.04

STATES <- 1:12
DIRECTION <- list(NORTH=0,EAST=1,SOUTH=2,WEST=3)

has_wall_north <- function(s) {  return(s %in% c( 1,   4,  7, 10, 6))  }
has_wall_east <- function(s)  {  return(s %in% c(10,  11, 12,  2   ))  }
has_wall_south <- function(s) {  return(s %in% c( 3,   6,  9, 12, 4))  }
has_wall_west <- function(s)  {  return(s %in% c( 1,   2,  3,  8   ))  }

step_will_suceed <- function() {  return(runif(1) > 0.2)  }

roll_norm <- function(dirs) { return(sample(dirs, size = 1)) }

roll_direction <- function(a) {
  if (a == DIRECTION$NORTH || a == DIRECTION$SOUTH)
    return(roll_norm( c(DIRECTION$WEST, DIRECTION$EAST) ))
  else
    return(roll_norm( c(DIRECTION$NORTH, DIRECTION$SOUTH) ))
}


step <- function(s, a) {
  dir <- a
  # nem arra megyunk amerre terveztuk
  if (!step_will_suceed()) {
    print(paste("STEP FAILED:", toString(s), toString(dir)))
    dir <- roll_direction(a)
  }
  step_to_final_diretion(s, dir)
}


concrete_step <- function(s, can_step, ds) { if(can_step(s)) return(list(ns = s, r = R)) else return(list(ns = s+ds, r = R)) }
step_north <- function(s) { return(concrete_step(s, has_wall_north, -1))  }
step_east  <- function(s) { return(concrete_step(s, has_wall_east,  +3))  }
step_south <- function(s) { return(concrete_step(s, has_wall_south, +1))  }
step_west  <- function(s) { return(concrete_step(s, has_wall_west,  -3))  }


step_to_final_diretion <- function(s, a) {
  #mostantol csak a fel lehet akadaly, tehat akkor nem lepunk, minden esetben lepunk
  if      (a == DIRECTION$NORTH) { return( step_north(s) ) }
  else if (a == DIRECTION$EAST)  { return( step_east(s)  ) }
  else if (a == DIRECTION$SOUTH) { return( step_south(s) ) }
  else                           { return( step_west(s)  ) }
}

observe <- function(policy, step, startstate, count) {
  o <- list(count=count, s=rep(0,count), a=rep(0,count), ns=rep(0,count), r=rep(0,count))
  s <- startstate
  for (i in 1:count) {
    a <- policy[[s]]
    res <- step(s, a)
    o$s[i]  <- s
    o$a[i]  <- a
    o$ns[i] <- res$ns
    o$r[i]  <- res$r
    s <- res$ns
  }
  return(o)
}

# policy<-matrix(list(), nrow=1, ncol=12)
# policy[1,] <- c(1, 0, 0, 1, -1, 3, 1, 0, 0, 3, 3, 3)
# o <- observe(policy, step, startstate = 1, 100)
# print(o)
# 
# 
# policyIteration <- function(env, iterations) {
#   policy <- array(1/env$actionCount,c(env$stateCount, env$actionCount))
#   for (i in 1:iterations) {
#     ret <- ADPagent(observe(policy, env, 1, 10000), policy)
#     policy <- greedyPolicy(ret$U, ret$T_ssa, env$actionCount)
#     policy <- epsilonGreedyPolicy(policy, 0.1)
#   }
#   return(policy)
# }
# 
# 
# greedyPolicy <- function(U, T_ssa, action_count) {
#   policy <- array(0, c(nrow(U), action_count))
#   for (s in 1:nrow(U)) {
#     #Us <- sapply(1:2, FUN=function(a) {obs <- env$step(s,a); u <- U[obs$ns]}) #!!!!
#     Us <- sapply( 1:2, FUN <- function(a) { T_ssa[s,,a] %*% U } )
#     a_best <- which.max(Us)
#     policy[s,a_best] <- 1
#   }
#   return(policy)
# }
# 
# 
# epsilonGreedyPolicy <- function(policy, epsilon) {
#   p <- policy
#   p[p==0] <- epsilon
#   p[p==1] <- (1-epsilon)
#   return(p)
# }

############
# SIMULATION
############

simulate_step <- function(s, a) {
  print(paste("STEP FROM:", toString(s), "DIR:", toString(a)))
  res <- step(s, a)
  s <- res$ns
  print(paste("STEP RESULT:", toString(s)))
  return(res)
}

simulate_steps <- function() {
  s <- 3
  simulate_count <- 100
  for(i in 1:simulate_count) {
    a <- sample(0:3, 1)
    res <- simulate_step(s, a)
    s <- res$ns
    if (s == 10) { stop(paste("WIN in step:", toString(i))) } 
    if (s == 11) { stop(paste("WIN in step:", toString(i))) }
    Sys.sleep(0.1)
  }
}
simulate_steps()

############
# TESTS
############

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