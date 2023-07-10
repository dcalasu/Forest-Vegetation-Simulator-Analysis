####Regular plots 


plot(US_UMB$Year,US_UMB$Crate,  main = "Change in carbon uptake US_UMB", 
     xlab = "Year", ylab = "Change in carbon uptake (MT C/Ha/Year)")
plot(US_MOz$Year,US_MOz$Crate, main = "Change in carbon uptake US_Moz", 
     xlab = "Year", ylab = "Change in carbon uptake (MT C/Ha/Year)")
plot(US_MMSF$Year,US_MMSF$Crate, main = "Change in carbon uptake US_MMSF", 
     xlab = "Year", ylab = "Change in carbon uptake (MT C/Ha/Year)")
plot(US_Ho1$Year,US_Ho1$Crate, main = "Change in carbon uptake US_Ho1", 
     xlab = "Year", ylab = "Change in carbon uptake (MT C/Ha/Year)")
plot(US_Ha1$Year,US_Ha1$Crate,  main = "Change in carbon uptake US_Ha1", 
     xlab = "Year", ylab = "Change in carbon uptake (MT C/Ha/Year)")
plot(US_Cwt$Year,US_Cwt$Crate,  main = "Change in carbon uptake US_Cwt", 
     xlab = "Year", ylab = "Change in carbon uptake (MT C/Ha/Year)")

##### ggplot year laber dont working
library(ggplot2)

# Combine the dataframes
combined_data <- rbind(US_UMB, US_MOz, US_MMSF, US_Ho1, US_Ha1, US_Cwt)
combined_data$Dataset <- rep(c("US_UMB", "US_MOz", "US_MMSF", "US_Ho1", "US_Ha1", "US_Cwt"), 
                             times = c(nrow(US_UMB), nrow(US_MOz), nrow(US_MMSF), 
                                       nrow(US_Ho1), nrow(US_Ha1), nrow(US_Cwt)))

#unique(combined_data$Year)
#colnames(combined_data)[7] <-"Ameriflux site"

# Plot using ggplot
ggplot(combined_data, aes(x = Year , y = Crate, color = Dataset, group = Dataset )) +
  geom_line(size = 1) +
  labs(title = "Change in carbon uptake", y = "Change in carbon uptake (MT C/Ha/Year)") +
  scale_color_manual(values = c("blue", "red", "green", "purple", "orange", "cyan")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

####general plot()_lines

# Set up the plotting area
US_Ha1$Year <- as.numeric(US_Ha1$Year)
US_Ho1$Year <- as.numeric(US_Ho1$Year)

# Set up the plot with appropriate limits and labels
plot(US_MMSF_FI_2023$Year, US_MMSF_FI_2023$Crate,type = "n",xlim = c(min(US_MMSF_FI_2023$Year), max(US_MMSF_FI_2023$Year)), ylim = c(0, 3),
     main = "Change in carbon uptake", xlab = "Year", ylab = "Biomass carbon uptake (MT C/Ha/Year)")
# Plot each dataset
lines(US_MMSF_FI_2023$Year, US_MMSF_FI_2023$Crate, col = "gray0", lwd = 4)
lines(US_MMSF_FI_2023_noH$Year, US_MMSF_FI_2023_noH$Crate, col = "darkgreen", lwd = 4)
lines(US_MMSF_FI_2023_defualtSI$Year, US_MMSF_FI_2023_defualtSI$Crate, col = "dodgerblue2", lwd = 4)
lines(US_UMB_FI_2023$Year, US_UMB_FI_2023$Crate, col = "gray", lwd = 4)
lines(US_UMB_FI_2023_noH$Year, US_UMB_FI_2023_noH$Crate, col = "green4", lwd = 4)
#lines(US_Ho1$Year, US_Ho1$Crate, col = "purple", lwd = 2)
#lines(US_Ha1$Year, US_Ha1$Crate, col = "orange", lwd = 2)
#lines(US_Cwt$Year, US_Cwt$Crate, col = "cyan", lwd = 2)

# Add legend
legend("topright", legend = c("US_MMSF", "US_MMSF no H", "US_MMSF Default SI","US_UMB", "US_UMB no H"),
       col = c("gray0", "darkgreen", "dodgerblue2","gray","green4"), lty = 1 , lwd = 2, cex = 0.8)


###Histogram 

hist(US_Ha1$Crate[2], main = "Change in carbon uptake", xlab = "Change in carbon uptake (MT C/Ha/Year)", ylim = c(0, 4), col = "lightblue")

# Add other histograms
hist(US_UMB$Crate[2], col = "blue", add = TRUE)
hist(US_MOz$Crate[2], col = "red", add = TRUE)
hist(US_MMSF$Crate[2], col = "green", add = TRUE)
#hist(MMSF_1997$Crate[2], col = "green4", add = TRUE)
hist(US_Ho1$Crate[2], col = "purple", add = TRUE)
hist(US_Ha1$Crate[2], col = "orange", add = TRUE)
hist(US_Cwt$Crate[2], col = "cyan", add = TRUE)

# Add legend
legend("topright", legend = c("US_UMB", "US_MOz", "US_MMSF", "US_MMSF_1997", "US_Ho1", "US_Ha1", "US_Cwt"),
       fill = c("blue", "red", "green", "green4", "purple", "orange", "cyan"))


#### bar plot
# Create bar plot
# Calculate mean and standard deviation for each dataset, excluding missing values
mean_values <- c(mean(US_UMB$Crate, na.rm = TRUE), 
                 mean(US_MOz$Crate, na.rm = TRUE), 
                 mean(US_MMSF$Crate, na.rm = TRUE),
                 mean(US_Ho1$Crate, na.rm = TRUE), 
                 mean(US_Ha1$Crate, na.rm = TRUE),
                 mean(US_Cwt$Crate, na.rm = TRUE))
std_dev <- c(sd(US_UMB$Crate, na.rm = TRUE), 
             sd(US_MOz$Crate, na.rm = TRUE), 
             sd(US_MMSF$Crate, na.rm = TRUE),
             sd(US_Ho1$Crate, na.rm = TRUE), 
             sd(US_Ha1$Crate, na.rm = TRUE),
             sd(US_Cwt$Crate, na.rm = TRUE))

# Remove NA values from the height and names arguments
mean_values <- mean_values[!is.na(mean_values)]
names_arg <- c("US_UMB","US_MOz", "US_MMSF","US_Ho1", "US_Ha1", "US_Cwt")
names_arg <- names_arg[!is.na(mean_values)]

# Calculate the lower and upper values for the error bars
lower_values <- mean_values - std_dev
upper_values <- mean_values + std_dev

# Calculate the midpoints for the error bars
midpoints <- (upper_values + lower_values) / 2

# Calculate the bar width
bar_width <- 0.8

# Create bar plot
barplot(height = c(US_UMB$Crate[2], 
                   US_MOz$Crate[2], 
                   US_MMSF$Crate[2],
                   US_Ho1$Crate[2], 
                   US_Ha1$Crate[2], 
                   US_Cwt$Crate[2]),
        names.arg = names_arg,
        col = c("red", "green", "green4", "purple", "orange", "cyan"),
        main = "Change in carbon uptake",
        xlab = "Dataset",
        ylab = "Change in carbon uptake (MT C/Ha/Year)",
        ylim = c(0, 4))

# Add error bars representing standard deviation
arrows(x0 = 1:length(mean_values), y0 = midpoints - std_dev, x1 = 1:length(mean_values), y1 = midpoints + std_dev,
       code = 3, angle = 90, length = 0.1)


# Add bars in the middle
rect(1:length(mean_values) - bar_width / 2, lower_values, 1:length(mean_values) + bar_width / 2, upper_values, col = "gray", border = NA)





library(ggplot2)

d <- data.frame(height = c(US_UMB$Crate[2], 
                           US_MOz$Crate[2], 
                           US_MMSF$Crate[2],
                           US_Ho1$Crate[2], 
                           US_Ha1$Crate[2], 
                           US_Cwt$Crate[2]),
                Site = names_arg, 
                height_sd = std_dev)
colorss <-  c("red", "green", "green4", "purple", "orange", "cyan")

ggplot()+
  geom_col(data = d, 
           aes(x = Site,
               y = as.numeric(height), 
               color = Site,
               fill = Site)) +
  geom_errorbar(data = d, 
                aes(x = Site,
                    ymin = as.numeric(height) - as.numeric(height_sd),
                    ymax = as.numeric(height) + as.numeric(height_sd)), 
                width = 0.5, color = "black") + 
  scale_color_manual(values = colorss) + 
  scale_fill_manual(values = colorss) + 
  ylab(expression("Change in carbon uptake (MT C/Ha/Year)")) +
  labs (title = "Projected change in carbon uptake FIA FVS")

# Add error bars representing standard deviation
#arrows(x0 = 1:length(mean_values), y0 = upper_values, x1 = 1:length(mean_values), y1 = lower_values,
#       code = 3, angle = 90, length = 0.1)
