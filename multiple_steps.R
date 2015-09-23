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
plot(step_size_vector, sd_vector)