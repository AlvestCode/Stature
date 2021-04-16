# Packages
library("dplyr")
library("pracma")
library("readxl")
library("ggplot2")
library("EnvStats")
library("PerformanceAnalytics")

# Data load
Alumnos <- read_excel("Alumnos.xlsx")
x = sapply(Alumnos[1:15,"Estatura"], as.numeric)
y = sapply(Alumnos[16:30,"Estatura"], as.numeric)

# Arithmetic mean
mean(Alumnos$"Estatura")

# Geometric mean
exp(mean(log(Alumnos$"Estatura")))

# Median 
median(Alumnos$"Estatura")

# Mode
Mode(Alumnos$"Estatura")

# Boxplot
ggplot(Alumnos, aes(x=Estatura)) + 
  stat_boxplot(geom ="errorbar") +
  geom_boxplot(fill="lightblue", varwidth = TRUE) +
  labs(x="Stature(Meters)") + 
  ggtitle("Stature boxplot") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(expand = c(0.7, 0))

# Density Curve
ggplot(Alumnos, aes(x=Estatura)) + 
  geom_density(fill="lightblue") +
  labs(x="Stature(Meters)", y = "Density") + 
  ggtitle("Stature density curve") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(expand = c(0.3, 0))
  
# Range 
max(Alumnos$"Estatura") - min(Alumnos$"Estatura")

# Variance
var(Alumnos$"Estatura")

# Standard deviation
sd(Alumnos$"Estatura")

# Coefficient of variation  
sd(Alumnos$"Estatura", na.rm = FALSE) / mean(Alumnos$"Estatura", na.rm = FALSE)

# Correspondence table
Alumnos2 <- read_excel("Alumnos2.xlsx")

Alumnos2$"Estatura" <- as.factor(Alumnos2$"Estatura")
Alumnos2$"Peso" <- as.factor(Alumnos2$"Peso")

levels(Alumnos2$"Estatura") <- c("1.21-1.22", "1.21-1.22", "1.24-1.25", "1.24-1.25",  
                                 "1.27-1.28", "1.27-1.28", "1.29-1.3",  "1.29-1.3")

levels(Alumnos2$"Peso") <- c("31-32", "31-32", "33", "34", "35")

Alumnos2 %>% select(Estatura, Peso) %>% table()

# Skewness
skewness(Alumnos$"Estatura", method = "fisher")

# Kurtosis
kurtosis(Alumnos$"Estatura", method = "excess")

# Shaphiro Normality Test 
shapiro.test(x)
shapiro.test(y)

# F-test
var.test(x, y)

# Confidence interval
t.test(x, y, 
       paired = FALSE,
       var.equal = TRUE,
       conf.level = 0.95)$conf.int 

# Hypothesis testing
t.test(x, y, 
       alternative = "two.sided", 
       paired = FALSE,
       var.equal = TRUE,
       conf.level = 0.95)
