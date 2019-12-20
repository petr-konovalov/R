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

describeBy(iris[,-5], iris$Species)

my_vector <- rnorm(30)
my_vector[sample(1:30, 10)] <- NA # на десять случайных позиций поместим NA

my_vector

fixed_vector <- replace(my_vector, is.na(my_vector), mean(my_vector, na.rm = T))

library(ggplot2)
data(airquality)

airquality$Month <- as.factor(airquality$Month)
ggplot(data = airquality, aes(x = Month, y = Ozone)) +
  geom_boxplot()

plot1 <- ggplot(data = mtcars, aes(x = mpg, y = disp, col = hp)) +
  geom_point()
ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(fill = Species))
  
red_men <- prop.table(HairEyeColor[,,'Male'], 2)['Red', 'Blue']
sum(HairEyeColor[,'Green','Female'])

library("ggplot2")
mydata <- as.data.frame(HairEyeColor[,,'Female'])
obj <- ggplot(data = mydata, aes(x = Hair, y = Freq, fill = Eye)) + 
geom_bar(stat="identity", position = 'dodge') + 
scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

binom.test(x = 7, n = 20)

chisq.test(HairEyeColor['Brown',,'Female'])

chisq.test(diamonds$cut, diamonds$color)$statistic

diamonds$factor_price = diamonds$price >= mean(diamonds$price)
diamonds$factor_carat = diamonds$carat >= mean(diamonds$carat)
main_stat = chisq.test(diamonds$factor_price, diamonds$factor_carat)$statistic

fisher.test(mtcars$am, mtcars$vs)$p.value

df = ToothGrowth
t_stat = t.test(subset(df, supp == 'OJ' & dose == 0.5)$len, subset(df, supp == 'VC' & dose == 2)$len)$statistic

df = read.csv('/home/petr/Рабочий стол/R/Stepik/lekarstva.csv')
t.test(df$Pressure_before, df$Pressure_after, paired = T)

setwd('/home/petr/Рабочий стол/R/Stepik')
df = read.table('dataset_11504_15.txt')
bartlett.test(V1 ~ V2, df)

df = read.table('dataset_11504_16.txt')
t.test(df$V1, df$V2)

summary(aov(yield ~ N + P + K, npk))

fit <- aov(Sepal.Width ~ Species, data = iris)
summary(fit)
TukeyHSD(fit)
  
df <- read.csv(url("https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv"))
View(df)
df$patient <- as.factor(df$patient)
summary(aov(temperature ~ pill + Error(patient/pill), data = df))
summary(aov(temperature ~ pill * doctor + Error(patient/(pill * doctor)), data = df))

#install.packages('devtools')
#require(devtools)
#install_version('Hmisc',  version = "4.1-0")
#install.packages("Hmisc")

library(ggplot2)
#library(Hmisc)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))

vect = c(1, 3, NA, NA, 4, NA, 5, 6, NA)

NA.position <- function(x){
  # put your code here  
  return(length((1:length(x))[is.na(x)]))
}

filtered.sum <- function(x){
  # put your code here  
  return(sum(x[x > 0]))
}

outliers.rm <- function(x){
  # put your code here  
  rg = 1.5 * IQR(x)
  q = quantile(x, probs = c(0.25, 0.75))
  return(x[-rg + q[1] <= x & x <= rg + q[2]])
}

filtered.cor <- function(x){
  maxAbs <- 0
  len <- length(x[1, ])
  filt <- logical(len)
  for (id in 1:len) {
    if (is.numeric(x[,id])) {
      filt[id] = T
    }
  }
  for (i in (1:len)[filt]) {
    for (j in (1:len)[filt]) {
      if (i != j) {
        curTst = as.numeric(cor.test(x[, i], x[, j])$estimate)
        if (abs(curTst) > maxAbs) {
          maxAbs <- curTst
        }
      }
    }
  }
  return(maxAbs)
}

filtered.cor <- function(x){
  tst <- corr.test(x[, sapply(x, is.numeric)])$r
  diag(tst) <- 0
  tst <- as.vector(tst)
  return(tst[which.max(abs(tst))])
}

test_data  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")
str(test_data)

test_data <- as.data.frame(list(col1 = c(-0.12, 0.57, -1.91, -1.02, -0.93, -1.93, -1.37, -1.4, 1.08, 1.61, 0.4, -1.35, -0.88, -1.53, 0.99, -1.62, 1.59, -1.94, 0.6, 1.08, 0.09, -1.55, -0.65, 0.34, 1.38, -0.83, 1.41, -1.41, -0.42, -1.8), col2 = c(-0.17, 0.34, -2.43, 2.06, -0.5, 0.58, 0.05, 0.65, 0.93, 0.67, 0.2, 1.21, 0.07, -2.13, 0.77, -0.08, -1.51, 0.53, 1.41, 0.08, -0.12, -0.02, 0.33, 1.29, -0.39, 0.23, -0.33, 0.55, 0.45, 1.96)))

smart_cor <- function(x) {
  if (shapiro.test(x[, 1])$p.value < 0.05 | shapiro.test(x[, 2])$p.value < 0.05) {
    return(as.numeric(cor.test(x[,1], x[,2], method = "spearman")$estimate))
  } else {
    return(as.numeric(cor.test(x[,1], x[,2])$estimate))
  }
}

smart_cor(test_data)

df <- read.table('dataset_11508_12.txt')
fff <- summary(lm(V1 ~ V2, df))

df <- ggplot2::diamonds
df1 <- subset(df, cut == 'Ideal' & carat == 0.48)

fit_coef <- lm(price ~ depth, subset(ggplot2::diamonds, cut == 'Ideal' & carat == 0.46))$coefficients