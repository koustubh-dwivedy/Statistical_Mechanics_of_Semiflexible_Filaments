step_size_vector=c(seq(from=10, to=100, by=10), seq(from=200, to=1000, by=100), seq(from=2000, to=5000, by=1000))
sd_vector=c()
for(step_size in step_size_vector){
  x_coord_vector=c()
  y_coord_vector=c()
  displacement_vector=c()
  for(trials in 1:10000){
    random_angle=runif(step_size)
    random_angle=random_angle*2*pi
    x_coord=0
    y_coord=0
    for(single_step in random_angle){
      x_coord = x_coord + cos(single_step)
      y_coord = y_coord + sin(single_step)
    }
    x_coord_vector=c(x_coord_vector, x_coord)
    y_coord_vector=c(y_coord_vector, y_coord)
    displacement_vector=sqrt(x_coord_vector*x_coord_vector + y_coord_vector*y_coord_vector)
    }
  sd_vector=c(sd_vector, sd(displacement_vector))
}
best_fit_line = lm(log(sd_vector)~log(step_size_vector))
best_fit_line #returns the following
#   Call:
#   lm(formula = log(sd_vector) ~ log(step_size_vector))
#
#   Coefficients:
#     (Intercept)  log(step_size_vector)  
#     -0.7912                 0.5030
plot(log(step_size_vector), log(sd_vector), xlab="log(step_size)", ylab ="log(sd_vector)", xlim = c(0, 10), ylim = c(0,3))
par(new = TRUE)
curve((-0.7912+0.5030*x), xlab = " ", ylab = " ", xlim = c(0, 10), ylim = c(0,3))