#Install the packages first


install.packages("pdftools")
install.packages("ggplot2")


library(pdftools)

# Extract text from the PDF
data_text<- pdf_text("C:\\Users\\Steve Kyalo\\Desktop\\Work\\2017NBADraft2.pdf")

# Spliting the text data into lines
data_lines <- strsplit(data_text, "\n")[[1]]

# Split the first line to get column names
column_names <- unlist(strsplit(data_lines[1], ","))

# Creating an empty data frame with the same number of columns
NBADRAFT2017 <- data.frame(matrix(ncol = length(column_names)))

# To Set column names
colnames(NBADRAFT2017) <- column_names

# Loop through the remaining lines and fill the data frame
for (i in 2:length(data_lines)) {
  row_data <- unlist(strsplit(data_lines[i], ","))
  NBADRAFT2017 <- rbind(NBADRAFT2017, row_data)
}

# Remove unnecessary double quotes
NBADRAFT2017[] <- lapply(NBADRAFT2017, function(x) gsub('"', '', x))

# Convert numeric columns to numeric data types
numeric_columns <- c("Height", "Wingspan", "Weight")
NBADRAFT2017[numeric_columns] <- lapply(NBADRAFT2017[numeric_columns], as.numeric)

# Words to be removed
words_to_remove <- c("Ntilikina", "Pasecniks", "Bolden", "Hartenstein", "Cancar", "Lessort", "Vezenkov", "Jaramaz", "Kaba", "Ferguson")

# Loop to remove words from the "Year" column
for (word in words_to_remove) {
  NBADRAFT2017$Year <- gsub(word, " ", NBADRAFT2017$Year)
}

#Finding the mean of wingspan 
column_mean <-  mean(NBADRAFT2017$Wingspan, na.rm =TRUE) 

#Filling the missing values with the mean
NBADRAFT2017$Wingspan <- ifelse(is.na(NBADRAFT2017$Wingspan), column_mean, NBADRAFT2017$Wingspan)

# Remove the first row from the data frame
NBADRAFT2017 <- NBADRAFT2017[-1, ]


print(NBADRAFT2017)

#Bar chart on the Position variable:

library(ggplot2)

ggplot(NBADRAFT2017, aes(x = Position)) +
 geom_bar() +
labs(title = "Distribution of Players by Position")


#Comparative bar chart comparing whether players went to college within each position:

ggplot(NBADRAFT2017, aes(x = Position, fill = College)) +
  geom_bar(position = "dodge") +
 labs(title = "2017 NBA Draft: College Attendance by Position")


# Create a comparative bar chart comparing position by class year (stacked bars)
ggplot(NBADRAFT2017, aes(x = Year,  fill = Position)) +
  geom_bar(position = "stack") +
  labs(title = "2017 NBA Draft: Position by Class Year")


#Pie chart on the Position variable

ggplot(NBADRAFT2017, aes(x = "", fill = Position)) +
  geom_bar(width = 1) +
  coord_polar("y") +
  labs(title = "Pie Chart of Player Position")




