#STEP1: preprocessing
ad_data <- read.csv("add.csv", header = FALSE, na.strings = "?", skip = 1)
ad_data <- ad_data[, -1]

colnames(ad_data) <- c(
  "height", "width", "ratio", "local",
  paste0("url_", 1:457),
  paste0("origurl_", 1:495),
  paste0("ancurl_", 1:472),
  paste0("alt_", 1:111),
  paste0("caption_", 1:19),
  "Class"
)

# Convert
ad_data[,1:1558] <- lapply(ad_data[,1:1558], as.numeric)
ad_data$local <- as.factor(ad_data$local)

ad_data$Class <- gsub("\\.", "", ad_data$Class)
ad_data$Class <- ifelse(ad_data$Class == "ad", 1, 0)
ad_data$Class <- as.factor(ad_data$Class)

# Fill NA
ad_data$height[is.na(ad_data$height)] <- median(ad_data$height, na.rm = TRUE)
ad_data$width[is.na(ad_data$width)] <- median(ad_data$width, na.rm = TRUE)
ad_data$local[is.na(ad_data$local)] <- 1

# Remove duplicate
ad_data <- ad_data[!duplicated(ad_data), ]

#Summary statistics
summary_table <- data.frame(
  Variable = c("Height", "Width"),
  Mean = c(mean(ad_data$height), mean(ad_data$width)),
  Median = c(median(ad_data$height), median(ad_data$width)),
  SD = c(sd(ad_data$height), sd(ad_data$width)),
  Min = c(min(ad_data$height), min(ad_data$width)),
  Max = c(max(ad_data$height), max(ad_data$width))
)

print(summary_table)

#Class distribution
class_table <- as.data.frame(table(ad_data$Class))
colnames(class_table) <- c("Class", "Frequency")

class_table$Percentage <- round(100 * class_table$Frequency / sum(class_table$Frequency), 2)

print(class_table)

#4.1 Histogram Height
p1 <- ggplot(ad_data, aes(x = height)) +
  geom_histogram(fill = "skyblue", bins = 30) +
  ggtitle("Figure 4.1: Histogram of Height") +
  theme_minimal()

#4.2 Histogram Width
p2 <- ggplot(ad_data, aes(x = width)) +
  geom_histogram(fill = "lightgreen", bins = 30) +
  ggtitle("Figure 4.2: Histogram of Width") +
  theme_minimal()

#4.3 Boxplot
p3 <- ggplot(ad_data) +
  geom_boxplot(aes(y = height), fill = "orange") +
  ggtitle("Figure 4.3: Boxplot of Height") +
  theme_minimal()

p4 <- ggplot(ad_data) +
  geom_boxplot(aes(y = width), fill = "pink") +
  ggtitle("Figure 4.4: Boxplot of Width") +
  theme_minimal()

#4.5 Scatter plot
p5 <- ggplot(ad_data, aes(x = height, y = width)) +
  geom_point(alpha = 0.5) +
  ggtitle("Figure 4.5: Height vs Width") +
  theme_minimal()

#4.6 Bar chart Class
p6 <- ggplot(ad_data, aes(x = Class)) +
  geom_bar(fill = "red") +
  ggtitle("Figure 4.6: Class Distribution") +
  theme_minimal()

#4.7 Bar chart Local
p7 <- ggplot(ad_data, aes(x = local)) +
  geom_bar(fill = "purple") +
  ggtitle("Figure 4.7: Local Distribution") +
  theme_minimal()

#4.8 Correlation Heatmap
cor_matrix <- cor(ad_data[, c("height", "width")])

corrplot(cor_matrix, method = "color", 
         title = "Figure 4.8: Correlation Matrix",
         mar = c(0,0,1,0))