unzip("./activity.zip")
data <- read.csv("./activity.csv")

install.packages("dplyr")
library(dplyr)

clean_data <- data
grouped <- group_by(clean_data, date)
summary <- summarise(grouped, steps_per_day = sum(steps, na.rm= TRUE))
hist(summary$steps_per_day, breaks = 20, main = "Steps per day" , xlab = "Number of Steps")


means_per_day <- summarise(grouped, mean_steps = mean(steps, na.rm= TRUE))
medians_per_day <- summarise(grouped, median_steps = median(steps, na.rm= TRUE))

mediam_of_the_total_steps <- median(summary[,"steps_per_day"][[1]], na.rm= TRUE)
mean_of_the_total_steps <- mean(summary[,"steps_per_day"][[1]], na.rm= TRUE)


grouped_interval <- group_by(clean_data, interval)
means_per_interval <- summarise(grouped_interval, mean_steps = mean(steps, na.rm= TRUE))
plot(means_per_interval, type = "l", ylab = "Average Steps")


max_per_interval <- filter(means_per_interval, mean_steps == summarise(means_per_interval, max = max(mean_steps, na.rm= TRUE))[[1]])

nas <- filter(data, is.na(steps))

number_of_nas = count(nas)[[1]]


merged <- merge(means_per_interval, nas, by.x = "interval", by.y = "interval")
nas_as_means <- select(mutate(select(merged, mean_steps, date, interval), steps = mean_steps), steps, date, interval)

no_nas_data <- filter(data, !is.na(steps))
all_data = rbind(no_nas_data, nas_as_means)

grouped2 <- group_by(all_data, date)
summary2 <- summarise(grouped2, steps_per_day = sum(steps))
hist(summary2$steps_per_day, breaks = 20, main = "Steps per day" , xlab = "Number of Steps")

means_per_day2 <- summarise(grouped2, mean_steps = mean(steps))
medians_per_day2 <- summarise(grouped2, median_steps = median(steps))

mediam_of_the_total_steps2 <- median(summary2[,"steps_per_day"][[1]])
mean_of_the_total_steps2 <- mean(summary2[,"steps_per_day"][[1]])


# Add weekdays
dev.off()
all_data$day_of_week <- ifelse(weekdays(as.Date(all_data$date)) == "Saturday" | weekdays(as.Date(all_data$date)) == "Sunday" ,"weekend","weekday")
all_data$day_of_week <- as.factor(all_data$day_of_week)

grouped_by_weekday <- group_by(all_data, day_of_week, interval)
summary_weekday <- summarise(grouped_by_weekday, steps = sum(steps))



install.packages("ggplot2")
library(ggplot2)


png("./weekday_steps.png")
plot <- qplot(interval, steps, data = summary_weekday, facets = day_of_week ~ ., geom = c("line"))
print(plot)
dev.off()