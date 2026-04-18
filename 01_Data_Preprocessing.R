ad_data <- read.csv(
  "C:/Users/User/Downloads/add.csv",
  header = FALSE,
  na.strings = "?",
  skip = 1
)
ad_data <- ad_data[ , -1]

colnames(ad_data) <- c(
  "height", "width", "ratio", "local",
  paste0("url_", 1:457),
  paste0("origurl_", 1:495),
  paste0("ancurl_", 1:472),
  paste0("alt_", 1:111),
  paste0("caption_", 1:19),
  "Class"
)

ad_data[] <- lapply(ad_data, trimws)

ad_data[ad_data == "?"] <- NA

ad_data[,1:1558] <- lapply(ad_data[,1:1558], as.numeric)

ad_data$local <- as.factor(ad_data$local)

ad_data$Class <- gsub("\\.", "", ad_data$Class)
ad_data$Class <- ifelse(ad_data$Class == "ad", 1, 0)
ad_data$Class <- as.factor(ad_data$Class)

which(colSums(is.na(ad_data)) > 0)
na_count <- colSums(is.na(ad_data))
na_cols <- na_count[na_count > 0]
na_ratio <- (na_cols / nrow(ad_data)) * 100
result <- data.frame(
  Column = names(na_cols),
  NA_Count = na_cols,
  NA_Ratio_Percent = round(na_ratio, 2)
)
print(result, row.names = FALSE)

ad_data$ratio <- NULL

library(ggplot2)
library(patchwork)

p1<-ggplot(ad_data, aes(x = height)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black",na.rm = TRUE) +
  theme_minimal() +
  labs(title = "Histogram of Height", x = "Height", y = "Frequency")

p2<-ggplot(ad_data, aes(y = height)) +
  geom_boxplot(fill = "lightblue", na.rm = TRUE) +
  theme_minimal() +
  labs(title = "Boxplot of Height", y = "Height")

p3<-ggplot(ad_data, aes(x = width)) +
  geom_histogram(bins = 30, fill = "salmon", color = "black",na.rm = TRUE) +
  theme_minimal() +
  labs(title = "Histogram of Width", x = "Width", y = "Frequency")

p4<-ggplot(ad_data, aes(y = width)) +
  geom_boxplot(fill = "salmon",na.rm = TRUE) +
  theme_minimal() +
  labs(title = "Boxplot of Width", y = "Width")

ad_data$local <- as.factor(ad_data$local)
p5<-ggplot(ad_data, aes(x = local,na.rm = TRUE)) +
  geom_bar(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Distribution of Local Variable",
       x = "Local (0 = external, 1 = internal)",
       y = "Count")
(p1 + p2) / (p3 + p4 ) / (plot_spacer() + p5 + plot_spacer())

ad_data$height[is.na(ad_data$height)] <- median(ad_data$height, na.rm = TRUE)
ad_data$width[is.na(ad_data$width)] <- median(ad_data$width, na.rm = TRUE)
ad_data$local[is.na(ad_data$local)] <- 1

calc_outlier <- function(x) {
  x <- na.omit(x)
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- IQR(x)
  outliers <- x[x < (Q1 - 1.5*IQR_val) | x > (Q3 + 1.5*IQR_val)]
  c(
    Count = length(outliers),
    Percent = round(length(outliers)/length(x)*100, 2)
  )
}

result <- rbind(
  height = calc_outlier(ad_data$height),
  width  = calc_outlier(ad_data$width)
)
print(result)

sum(duplicated(ad_data))

ad_data <- ad_data[!duplicated(ad_data), ]

ad_data[duplicated(ad_data), ][1:5, 1:10]

str(ad_data, list.len = 10)
