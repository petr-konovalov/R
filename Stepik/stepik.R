mtcars$new_var <- ifelse(mtcars$carb >= 4 | mtcars$cyl > 6, 1, 0)
ts (1:12, frequency = 12, start = 1990)
v <- c(1, 3, 2, 7, 11, 5)
v <-as.vector(AirPassengers)
v[2:length(v)][v[2:length(v)] > v[1:(length(v)-1)]]

v <- c(0, cumsum(as.vector(AirPassengers)))
moving_average <- (v[-(1:10)] - v[-((length(v) - 9): length(v))]) / 10

descriptions_stat <- aggregate(cbind(mtcars$hp, mtcars$disp) ~ mtcars$am, mtcars, sd)
descriptions_stat

aggregate(Ozone ~ Month, data = airquality[7 <= airquality$Month & airquality$Month <= 9, ], length)

describeBy(airquality, airquality$Month)

