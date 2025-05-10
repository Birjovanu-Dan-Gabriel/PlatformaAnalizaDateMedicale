library(ggplot2)

# Încărcare date
health_data <- read.csv("data/raw_data.csv")

# Analiză rapidă
summary(health_data)
table(health_data$region, health_data$smoker)
ggplot(health_data, aes(x=age, y=bmi, color=gender)) + geom_point()