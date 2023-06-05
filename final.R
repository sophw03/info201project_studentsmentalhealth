# Libraries
library(dplyr)
library(stringr)
library(ggplot2)

# Read the data
df_1 <- read.csv("StudentMentalHealth.csv")
df_2 <- read.csv("Mental_Health_Care_in_the_Last_4_Weeks.csv")

# Merge the dfs
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

# Change Year to lowercase 
final_df$Your.current.year.of.Study <- tolower(final_df$Your.current.year.of.Study)
final_df$Your.current.year.of.Study <- gsub("year", "year", final_df$Your.current.year.of.Study)

# Created grade level column based on year of current study
final_df$grade_level <- factor(final_df$Your.current.year.of.Study,
                               levels = c("year 1", "year 2", "year 3", "year 4"),
                               labels = c("Freshman", "Sophomore", "Junior", "Senior"))

# Convert College_Major to lowercase to create similar groupings
final_df$College_Major <- tolower(final_df$College_Major)

# Create a simpler category for majors 
categories <- c("Humanities", "Sciences", "Engineering", "Business", "Other")
patterns <- c("islamic education|pendidikan islam|irkhs|usuluddin|fiqh",
              "mathemathics|marine science|biomedical science|biotechnology",
              "engineering|enm|engine|engin",
              "bit|bcs|human resources|accounting|banking studies|business administration|econs|cts",
              ".*")

# function to assign category based on the patterns
assign_category <- function(major) {
  for (i in seq_along(patterns)) {
    if (grepl(patterns[i], major)) {
      return(categories[i])
    }
  }
  return("Other")
}

# new column for category
final_df$category <- sapply(final_df$College_Major, assign_category)

# Calculate the total count of people in each category
total_counts_category <- table(final_df$category)

# Calculate the people with depression, anxiety, and panic attacks in each major
depression_counts_major <- table(final_df$category, final_df$Do.you.have.Depression., final_df$Do.you.have.Anxiety., final_df$Do.you.have.Panic.attack.)

# Calculate the total count of people in each major
total_counts_major <- table(final_df$category)

# Calculate the percentage of people with depression within each major
percentage_depression_major <- depression_counts_major[, , , "Yes"] / rowSums(depression_counts_major) * 100

# Calculate the people with anxiety in each major
anxiety_counts_major <- table(final_df$category, final_df$Do.you.have.Anxiety.)
percentage_anxiety_major <- anxiety_counts_major[, "Yes"] / total_counts_major * 100

# Calculate the people with panic attacks in each major
panic_counts_major <- table(final_df$category, final_df$Do.you.have.Panic.attack.)
percentage_panic_attacks_major <- panic_counts_major[, "Yes"] / total_counts_major * 100

# df with the results for major
summary_data_major <- data.frame(Major = rownames(percentage_depression_major),
                                 Percentage_Depression = as.numeric(percentage_depression_major),
                                 Percentage_Anxiety = as.numeric(percentage_anxiety_major),
                                 Percentage_Panic_Attacks = as.numeric(percentage_panic_attacks_major))
summary_data_major$category <- summary_data_major$Major

# breaks for age groups
breaks <- c(18, 19, 21, 23, 25)

# labels for age groups
labels <- c("18", "19-20", "21-22", "23-24")

# Add Age_Group
final_df$Age_Group <- cut(final_df$Age, breaks = breaks, labels = labels)

# Create new continuous/numerical variable: Age_Group_Num
final_df$Age_Group_Num <- as.numeric(final_df$Age_Group)

# Calculate the number of unique age groups
num_age_groups <- length(unique(final_df$Age_Group))

# Generate a sequence of values based on the number of age groups
replacement_values <- seq(1, num_age_groups)

# replacement values to Age_Group_Num
final_df$Age_Group_Num <- replacement_values

# Calculate the people with depression in each age group
depression_counts_age <- table(final_df$Age_Group, final_df$Do.you.have.Depression)

# Calculate the total count of people in each age group
total_counts_age <- table(final_df$Age_Group)

# Calculate the percentage of people with depression within each age group
percentage_depression_age <- depression_counts_age[, "Yes"] / total_counts_age * 100

# Create a df with the results for age
summary_data_age <- data.frame(Age_Group = rownames(percentage_depression_age),
                               Percentage_Depression = as.numeric(percentage_depression_age))
# Calculate the percentage of people with depression within each age group
percentage_depression_age <- depression_counts_age[, "Yes"] / total_counts_age * 100

# Calculate the people with anxiety in each age group
anxiety_counts_age <- table(final_df$Age_Group, final_df$Do.you.have.Anxiety.)
percentage_anxiety_age <- anxiety_counts_age[, "Yes"] / total_counts_age * 100

# Calculate the people with panic attacks in each age group
panic_counts_age <- table(final_df$Age_Group, final_df$Do.you.have.Panic.attack.)
percentage_panic_attacks_age <- panic_counts_age[, "Yes"] / total_counts_age * 100

# Create a df with the results for age
summary_data_age <- data.frame(Age_Group = rownames(percentage_depression_age),
                               Percentage_Depression = as.numeric(percentage_depression_age),
                               Percentage_Anxiety = as.numeric(percentage_anxiety_age),
                               Percentage_Panic_Attacks = as.numeric(percentage_panic_attacks_age))
summary_data_age$category <- summary_data_age$Age_Group

# Calculate the people with depression, anxiety, and panic attacks in each GPA range
depression_counts_gpa <- table(final_df$What.is.your.CGPA., final_df$Do.you.have.Depression)
anxiety_counts_gpa <- table(final_df$What.is.your.CGPA., final_df$Do.you.have.Anxiety.)
panic_counts_gpa <- table(final_df$What.is.your.CGPA., final_df$Do.you.have.Panic.attack.)

# Calculate the total count of people in each GPA range
total_counts_gpa <- table(final_df$What.is.your.CGPA.)

# Calculate the percentage of people with depression, anxiety, and panic attacks within each GPA range
percentage_depression_gpa <- depression_counts_gpa[, "Yes"] / total_counts_gpa * 100
percentage_anxiety_gpa <- anxiety_counts_gpa[, "Yes"] / total_counts_gpa * 100
percentage_panic_attacks_gpa <- panic_counts_gpa[, "Yes"] / total_counts_gpa * 100

# Create the df with the GPA range and percentages
summary_data_gpa <- data.frame(
  GPA_Range = c("0 - 1.99", "2.00 - 2.49", "2.50 - 2.99", "3.00 - 3.49", "3.50 - 4.00", "Unknown"),
  Percentage_Depression = as.numeric(percentage_depression_gpa),
  Percentage_Anxiety = as.numeric(percentage_anxiety_gpa),
  Percentage_Panic_Attacks = as.numeric(percentage_panic_attacks_gpa)

)


# Create bar plot for total summary 
summary_data_major <- data.frame(
  Major = c("Business", "Engineering", "Humanities", "Other", "Sciences"),
  Percentage_Depression = c(8.333333, 20.512821, 7.142857, 19.127517, 0.000000),
  Percentage_Anxiety = c(46.5053763, 25.2136752, 15.7142857, 0.3681946, 8.5365854),
  Percentage_Panic_Attacks = c(32.7956989, 32.0512821, 25.0000000, 0.5874566, 8.5365854)
)

