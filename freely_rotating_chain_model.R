angle_size_vector=c(seq(from=10, to=90, by=10))
sd_vector=c()
displacement = c()
for(angle_size in angle_size_vector){
  x_coord_vector=c()
  y_coord_vector=c()
  displacement_vector=c()
  for(trials in 1:10000){
    random_angle=round(runif(100))
    for(i in seq(1:100)){
      if (random_angle[i] == 0){
        random_angle[i] = -1
      }
    }
    random_angle=random_angle*angle_size*pi/180
    x_coord=0
    y_coord=0
    angle_summation = 0
    for(single_step in seq(1:100)){
      angle_summation = angle_summation + random_angle[single_step]
      x_coord = x_coord + cos(angle_summation)
      y_coord = y_coord + sin(angle_summation)
    }
    x_coord_vector=c(x_coord_vector, x_coord)
    y_coord_vector=c(y_coord_vector, y_coord)
    displacement_vector=sqrt(x_coord_vector*x_coord_vector + y_coord_vector*y_coord_vector)
    }
  sd_vector=c(sd_vector, sd(displacement_vector))
  displacement[angle_size/10] = list(displacement_vector)
}