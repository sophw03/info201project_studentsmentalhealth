# Libraries
library(dplyr)
library(stringr)
library(ggplot2)

# Read the data
df_1 <- read.csv("Student Mental Health.csv")
df_2 <- read.csv("Mental_Health_Care_in_the_Last_4_Weeks.csv")

# Merge the data frames
df_2 <- df_2[df_2$Group == "By Age",]
df_2 <- df_2[df_2$Subgroup == "18 - 29 years",]
for (x in 1:nrow(df_2)){
  age <- round(runif(1, 18, 29),)
  df_2[x, "Subgroup"] <- age
}

# Create subgroup column in df_2
df_2$Age_Subgroup <- round(runif(nrow(df_2), 18, 29))

# Merge the dfs by age 
combo_df <- merge(df_1, select(df_2, Age_Subgroup), by.x = "Age", by.y = "Age_Subgroup", all.x = TRUE)
final_df <- combo_df[1:25000, ]


# Rename column "What.is.your.course." to "College_Major" to look cleaner 
colnames(final_df)[colnames(final_df) == "What.is.your.course."] <- "College_Major"

# Change Year to lowercase to replace variations
final_df$Your.current.year.of.Study <- tolower(final_df$Your.current.year.of.Study)
final_df$Your.current.year.of.Study <- gsub("year", "year", final_df$Your.current.year.of.Study)

# Created grade_level column based on year of current study
final_df$grade_level <- factor(final_df$Your.current.year.of.Study,
                               levels = c("year 1", "year 2", "year 3", "year 4"),
                               labels = c("Freshman", "Sophomore", "Junior", "Senior"))


# Calculate total count of people in each grade level
total_counts <- table(final_df$grade_level)

# Convert College_Major to lowercase to create similar groupings
final_df$College_Major <- tolower(final_df$College_Major)

# Define categories and corresponding regular expressions
categories <- c("Humanities", "Sciences", "Engineering", "Business")
patterns <- c("islamic education|pendidikan islam|irkhs|usuluddin|fiqh",
              "mathemathics|marine science|biomedical science|biotechnology",
              "engineering|enm|engine|engin",
              "bit|bcs|human resources|accounting|banking studies|business administration|econs|cts")

# Creates Function to assign category based on the patterns
assign_category <- function(major) {
  for (i in seq_along(patterns)) {
    if (grepl(patterns[i], major)) {
      return(categories[i])
    }
  }
  return("Other")
}

# Add new column for category
final_df$category <- sapply(final_df$College_Major, assign_category)

# Calculate the total of people in each category
total_counts_category <- table(final_df$category)

# Calculate the people with depression in each major
depression_counts_major <- table(final_df$category, final_df$Do.you.have.Depression.)

# Calculate the total of people in each major
total_counts_major <- total_counts_category

# Calculate the percentage of people with depression within each major
percentage_depression_major <- depression_counts_major[, "Yes"] / total_counts_major * 100

# Create df with the results for major
summary_data_major <- data.frame(Major = names(percentage_depression_major),
                                 Percentage_Depression = as.numeric(percentage_depression_major))

# Calculate people with depression in each age group
depression_counts_age <- table(final_df$Age, final_df$Do.you.have.Depression.)

# Calculate the total of people in each age group
total_counts_age <- table(final_df$Age)

# Calculate percentage of people with depression within each age group
percentage_depression_age <- depression_counts_age[, "Yes"] / total_counts_age * 100

# Create df with the results for age
summary_data_age <- data.frame(Age_Group = names(percentage_depression_age),
                               Percentage_Depression = as.numeric(percentage_depression_age))

# Calculate the people with depression in each GPA range
depression_counts_gpa <- table(final_df$What.is.your.CGPA., final_df$Do.you.have.Depression.)

# Calculate the total of people in each GPA range
total_counts_gpa <- table(final_df$What.is.your.CGPA.)

# Calculate percentage of people with depression within each GPA range
percentage_depression_gpa <- depression_counts_gpa[, "Yes"] / total_counts_gpa * 100

# Create df with the results for GPA
summary_data_gpa <- data.frame(GPA_Range = names(percentage_depression_gpa),
                               Percentage_Depression = as.numeric(percentage_depression_gpa))

# Create new categorical variable called Major_Category to summarize 
major_categories <- c("Humanities", "Sciences", "Engineering", "Business", "Other")
final_df$Major_Category <- factor(final_df$category, levels = major_categories)

# Create new continuous/numerical variable: Age_Group_Num
final_df$Age_Group_Num <- as.numeric(final_df$Age)

# Create a summary data frame for major
summary_data_major <- aggregate(Do.you.have.Depression. ~ Major_Category, data = final_df, FUN = function(x) mean(x == "Yes") * 100)
colnames(summary_data_major) <- c("Major_Category", "Percentage_Depression")

# Print the summary data frame for major
print(summary_data_major)

# Create Visualizations 
# Plot the data for major
ggplot(summary_data_major, aes(x = Major_Category, y = Percentage_Depression)) +
  geom_bar(stat = "identity", fill = "lightpink") +
  labs(title = "Percentage of People with Depression by Major Category",
       x = "Major Category",
       y = "Percentage of People with Depression")


# Plot the data for age
ggplot(summary_data_age, aes(x = Age_Group, y = Percentage_Depression)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Percentage of People with Depression by Age Group",
       x = "Age Group",
       y = "Percentage of People with Depression")


# Plot the data for GPA
ggplot(summary_data_gpa, aes(x = GPA_Range, y = Percentage_Depression)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Percentage of People with Depression by GPA Range",
       x = "GPA Range",
       y = "Percentage of People with Depression")
