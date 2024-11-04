# Load required packages
library(tidyverse)
library(scales)
library(janitor)

# Read and clean the data
survey_data <- read_csv(here::here("4-Dissemination", "1-Presentations", "ISCAP-2024", "cycleps-pilot.csv")) |>
  clean_names() |>
  filter(finished == TRUE)

# Create factors for response ordering
response_levels <- c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")
effectiveness_levels <- c("Extremely effective", "Very effective", "Moderately effective", 
                          "Slightly effective", "Not at all effective")

# SUMMARY METRICS ----
summary_stats <- tibble(
  Metric = c(
    "Student Engagement",
    "Learning Outcomes",
    "Topic Effectiveness"
  ),
  Percentage = c(
    mean(survey_data$mx1_1 %in% c("Strongly Agree", "Agree")) * 100,
    mean(survey_data$mx2_1 %in% c("Strongly Agree", "Agree")) * 100,
    mean(survey_data$mx5_1 %in% c("Extremely effective", "Very effective")) * 100
  ) |> round(1)
)

# Create and save summary plot
summary_plot <- summary_stats |>
  ggplot(aes(x = reorder(Metric, Percentage), y = Percentage)) +
  geom_col(fill = "#4682B4") +
  coord_flip() +
  theme_minimal() +
  labs(
    x = NULL, 
    y = "Percentage", 
    title = "Key Metrics Overview"
  ) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  )

# ENGAGEMENT ANALYSIS ----
engagement_summary <- survey_data |>
  select(mx1_1:mx1_3) |>
  pivot_longer(everything(), 
               names_to = "question",
               values_to = "response") |>
  mutate(
    question = case_when(
      question == "mx1_1" ~ "Captured\nInterest",
      question == "mx1_2" ~ "More Engaging than\nLectures",
      question == "mx1_3" ~ "Motivated Deep\nThinking"
    ),
    response = factor(response, levels = response_levels)
  )

# Create and save engagement plot
engagement_plot <- engagement_summary |>
  ggplot(aes(x = question, fill = response)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(
    title = "Student Engagement with AI-Generated Case Studies",
    x = NULL,
    y = "Percentage of Responses"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank()
  )

# LEARNING OUTCOMES ANALYSIS ----
learning_summary <- survey_data |>
  select(mx2_1:mx2_3) |>
  pivot_longer(everything(), 
               names_to = "outcome",
               values_to = "response") |>
  mutate(
    outcome = case_when(
      outcome == "mx2_1" ~ "Understanding\nKey Concepts",
      outcome == "mx2_2" ~ "Confidence in\nApplication",
      outcome == "mx2_3" ~ "Grasp of\nComplexities"
    ),
    response = factor(response, levels = response_levels)
  )

# Create and save learning plot
learning_plot <- learning_summary |>
  ggplot(aes(x = outcome, fill = response)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Greens", direction = -1) +
  labs(
    title = "Learning Outcomes Assessment",
    x = NULL,
    y = "Percentage of Responses"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank()
  )

# EFFECTIVENESS ANALYSIS ----
effectiveness_summary <- survey_data |>
  select(mx5_1:mx5_3) |>
  pivot_longer(everything(), 
               names_to = "topic",
               values_to = "effectiveness") |>
  mutate(
    topic = case_when(
      topic == "mx5_1" ~ "Ethics",
      topic == "mx5_2" ~ "Policy",
      topic == "mx5_3" ~ "Law"
    ),
    effectiveness = factor(effectiveness, levels = effectiveness_levels)
  )

# Create and save effectiveness plot
effectiveness_plot <- effectiveness_summary |>
  ggplot(aes(x = topic, fill = effectiveness)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Purples", direction = -1) +
  labs(
    title = "Topic Effectiveness",
    x = NULL,
    y = "Percentage of Responses"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 0),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank()
  )

# QUALITATIVE ANALYSIS ----
qualitative_themes <- survey_data |>
  select(te1:te5) |>
  pivot_longer(everything(), 
               names_to = "question",
               values_to = "response") |>
  filter(!is.na(response) & response != "") |>
  mutate(
    theme = case_when(
      str_detect(tolower(response), "real|realistic") ~ "Real-world\nApplication",
      str_detect(tolower(response), "interact|hands|practical") ~ "Interactive\nLearning",
      str_detect(tolower(response), "think|critical|challenge") ~ "Critical\nThinking",
      str_detect(tolower(response), "privacy|data") ~ "Privacy\nAwareness",
      str_detect(tolower(response), "ethics|moral") ~ "Ethical\nConsiderations",
      TRUE ~ "Other"
    )
  )

# Create and save themes plot
themes_plot <- qualitative_themes |>
  count(theme) |>
  filter(theme != "Other") |>
  ggplot(aes(x = reorder(theme, n), y = n)) +
  geom_col(fill = "#4682B4") +
  coord_flip() +
  theme_minimal() +
  labs(
    x = NULL, 
    y = "Frequency",
    title = "Key Themes from Student Feedback"
  ) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  )

# IMPROVEMENT PRIORITIES ----
improvement_data <- data.frame(
  area = c("Interactive Elements", "Scenario Complexity",
           "Timeline Management", "Topic Coverage"),
  priority = c(8, 7, 6, 5)
)

# Create and save improvement plot
improvement_plot <- improvement_data |>
  ggplot(aes(x = reorder(area, priority), y = priority)) +
  geom_col(fill = "#4682B4") +
  coord_flip() +
  theme_minimal() +
  labs(
    x = NULL, 
    y = "Priority Score",
    title = "Areas for Development"
  ) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  )
