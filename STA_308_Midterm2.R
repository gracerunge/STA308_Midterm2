################################
#* STA 308 Fall 2023 Midterm 2 
#* 
#* Author: Grace Runge 
#* 
#* Date: 11/17/2023
#* 
#* Purpose: To write script which 
#*    answers the required questions
#*    for our second midterm. 
################################


# Stated problem: As coach, you get to decide: 
#   does your team go for a two point field goal or a three 
#   point field goal? Your code must consider both scenarios.

# To tackle this problem, I will first write a simulation function to 
#    predict different point total outcomes when going for two points.


two_point_sim <- function(){
  points <- 0
  inbound_pass <- sample(c("Complete", "Incomplete"), size=1, prob=c(0.65, 0.35))
  if(inbound_pass=="Complete"){
    fouled <- sample(c("Fouled", "Not Fouled"), size=1, prob=c(0.1, 0.9))
    if(fouled=="Fouled"){
      free_shot <- sample(c("Made", "Missed"), size=1, prob=c(0.55, 0.45))
      if(free_shot=="Made"){
        points <- points + 1
      } else{
        points <- points + 0 
      }
      free_shot2 <- sample(c("Made", "Missed"), size=1, prob=c(0.55, 0.45))
      if(free_shot2=="Made"){
        points <- points + 1
      } else{
        points <- points + 0 
      }
    } else {
      makes_shot <- sample(c("Made", "Missed"), size=1, prob=c(0.52, 0.48))
      if(makes_shot=="Made"){
        points <- 2
      } else {
        points <- 0
      }
    }
  }else{ 
    points <- 0
  }
  points
}
  

# Next write a simulation function to predict outcomes when going for three

  
three_point_sim <- function(){
  points <- 0
  inbound_pass <- sample(c("Complete", "Incomplete"), size=1, prob=c(0.8, 0.2))
  if(inbound_pass=="Complete"){
    fouled <- sample(c("Fouled", "Not Fouled"), size=1, prob=c(0.05, 0.95))
    if(fouled=="Fouled"){
      free_shot1 <- sample(c("Made", "Missed"), size=1, prob=c(0.55, 0.45))
      if(free_shot1=="Made"){
        points <- points + 1
      } else{
        points <- points + 0 
      }
      free_shot2 <- sample(c("Made", "Missed"), size=1, prob=c(0.55, 0.45))
      if(free_shot2=="Made"){
        points <- points + 1
      } else{
        points <- points + 0 
      }
      free_shot3 <- sample(c("Made", "Missed"), size=1, prob=c(0.55, 0.45))
      if(free_shot2=="Made"){
        points <- points + 1
      } else{
        points <- points + 0 
      }
    } else {
      makes_shot <- sample(c("Made", "Missed"), size=1, prob=c(0.4, 0.8))
      if(makes_shot=="Made"){
        points <- 3
      } else {
        points <- 0
      }
    }
  }else{ 
    points <- 0
  }
  points
}


#* For each scenario, approximate the mean number 
#*   of points scored. Also approximate the SD.
#*   Comment on these findings.

vec2 <- c()
for(i in 1:100000){
  vec2<- c(vec2, two_point_sim())
}
mean(vec2)
sd(vec2)


vec3 <- c()
for(i in 1:100000){
  vec3<- c(vec3, three_point_sim())
}
mean(vec3)
sd(vec3)

#* When going for two points (and 100,000 simulations ran) the mean points 
#*   scored is 0.68 points with a standard deviations of around 0.9
#* When going for three points (and 100,000 simulations ran) the mean points 
#*    scored is 0.83 with a standard deviation of 1.3.
#*    
#* Based on these findings there are benefits to going for each situation.
#*   If you go for two points you will score less points on average (0.68 compared
#*   to 0.83) but the standard deviation is much lower than when going for 3. 
#*   When going for three points there is more variation in how many points 
#*   could be scored, you could win big and score more than you need to just 
#*   win by one. So if you're into the glory of that, then I'd go for three. 


#*From your results, approximate the probabilities that:
#*   you win the game for each strategy
#*   the game ends in a tie (you score 1 point) and it will head to overtime
#*   you lose the game during regulation 
#*   Which strategy is preferred under these assumptions?
#*   
#*   NOTE: Since I did not set a seed in the beginning, the exact percentages 
#*   change slightly each time it is run and will when it is run by another 
#*   person as well, but I ran each simulation 3x and the numbers stayed within 
#*   about 1% of what I have listed for all three times.

#probability of winning the game when you go for two is about 32%:
num <- 0
for(i in vec2){
  if(i>1){
    num <- num + 1
  } else {
    num <- num
  }
}

win_prob2 <- num / length(vec2) *100
win_prob2

#probability of winning the game when you go for three is about 27.5%:
num <- 0
for(i in vec3){
  if(i>1){
    num <- num + 1
  } else {
    num <- num
  }
}

win_prob3 <- num / length(vec3) *100
win_prob3


#probability of losing the game when you go for two is about 64%:
num <- 0
for(i in vec2){
  if(i<1){
    num <- num + 1
  } else {
    num <- num
  }
}

lose_prob2 <- num / length(vec2) *100
lose_prob2


#probability of losing  the game when you go for three is about 71%:
num <- 0
for(i in vec3){
  if(i<1){
    num <- num + 1
  } else {
    num <- num
  }
}

lose_prob3 <- num / length(vec3) *100
lose_prob3


#probability of the game ending in a tie when you go for two is about 3%:
num <- 0
for(i in vec2){
  if(i==1){
    num <- num + 1
  } else {
    num <- num
  }
}

tie_prob2 <- num / length(vec3) *100
tie_prob2


#probability of the game ending in a tie when you go for three is about 1%:
num <- 0
for(i in vec3){
  if(i==1){
    num <- num + 1
  } else {
    num <- num
  }
}

tie_prob3 <- num / length(vec3) *100
tie_prob3


#* Based on these numbers I would chose to go for two points. The probability 
#*    of winning when going for two is about 5% greater than when going 
#*    for three. There is a higher chance that the game 
#*    will end in a tie if you go for two compared to going for three, but I 
#*    think it is better to end in a tie and have to go into overtime than to 
#*    risk losing in regular time by going for three.

#* BONUS:
#* If the game ends in a tie, the two teams will play a 5-minute 
#*    overtime period. Suppose the probability of winning the game 
#*    in overtime is 50%. Would this change your decision?

# I added default probabilities based on what I found above, but those 
#  could of course be overwritten with other probabilities if you find 
#  different numbers than I do when running the simulations.

# Note: 1=win, 0=loss

ot2 <- function(probTie=0.03, probWin=0.32){
  win_lose <- 0
  end_in_tie <- sample(c("Tie", "No Tie"), size=1, prob=c(probTie, 1-probTie))
  if(end_in_tie=="Tie"){
    win_lose < - sample(c(1, 0), size=1, prob=c(0.5, 0.5))
  }else{
    win_lose <- sample(c(1, 0), size=1, prob=c(probWin, 1-probWin))
  }
  win_lose 
}
  
vec_tie2 <- c()
for(i in 1:100000){
  vec_tie2<- c(vec_tie2, ot2())
}
mean(vec_tie2)
sd(vec_tie2)

ot3 <- function(probTie=0.01, probWin=0.275){
  win_lose <- 0
  end_in_tie <- sample(c("Tie", "No Tie"), size=1, prob=c(probTie, 1-probTie))
  if(end_in_tie=="Tie"){
    win_lose < - sample(c(1, 0), size=1, prob=c(0.5, 0.5))
  }else{
    win_lose <- sample(c(1, 0), size=1, prob=c(probWin, 1-probWin))
  }
  win_lose 
}

vec_tie3 <- c()
for(i in 1:100000){
  vec_tie3<- c(vec_tie3, ot3())
}
mean(vec_tie3)
sd(vec_tie3)

#* Including the probabilities of winning and losing in overtime, when going
#*   for two, you will still win about 31% of the time. When going for three you 
#*   will still win about 27% of the time so I would not change my decision
#*   knowing that there is a 50% chance to win in overtime.










  