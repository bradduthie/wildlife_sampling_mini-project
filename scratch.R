




x1 <- 23;
x2 <- 83;
y1 <- 58;
y2 <- 23;

plot(x = c(x1, x2), y = c(y1, y2), asp = 1, 
     xlim = c(0, 100), ylim = c(0, 100));

# Get the slop of the student transect
mod <- lm(c(y1, y2) ~ c(x1, x2));
icp <- as.numeric(mod$coefficients[1]);
slp <- as.numeric(mod$coefficients[2]);

abline(a = icp, b = slp);

# Get the perpendicular line
isl <- (-1 * slp)^-1;

parrr <- function(isl, x1, y1){
  bb <- y1 - (isl * x1);
  return(bb);
}

new_x1 <- 40;
new_y1 <- 0;
points(x = new_x1, y = new_y1, pch = 20, col = "red")

p_int   <- parrr(isl, new_x1, new_y1);

abline(a = p_int, b = isl)


x_inter <- (p_int - icp) / (slp - isl)
y_inter <- icp + x_inter * slp;

points(x = x_inter, y = y_inter, pch = 20, col = "blue", cex = 2);

p_dist <- sqrt((new_x1 - x_inter)^2 + (new_y1 - y_inter)^2)

# Now check to see if the x_inter is between steps in the x dimension
# If so, then take the distance seriously if it is below 10

get_parallel <- function(x1, x2, y1, y2){
  mod <- lm(c(y1, y2) ~ c(x1, x2));
  icp <- as.numeric(mod$coefficients[1]);
  slp <- as.numeric(mod$coefficients[2]);
  if(!is.na(slp) & slp != 0){
    pll <- (-1 * slp)^-1;
  }
  if(!is.na(slp) & slp == 0){
    pll <- -2
  }
  if(is.na(slp)){
    pll <- -2;
  }
  
  
  return(c(slp, pll));
}
