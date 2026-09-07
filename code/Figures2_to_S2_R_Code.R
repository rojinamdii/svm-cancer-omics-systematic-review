# ================================================================
# Descriptive figures for a systematic review WITHOUT meta-analysis
# Source workbook: data/Supplementary_Data_File_1.xlsx
# Figures summarize extracted study characteristics only.
# No pooled performance estimate, hypothesis test, regression,
# meta-analysis, or statistical comparison is performed.
# ================================================================

required_packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "stringr",
  "forcats", "patchwork", "scales"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
library(patchwork)
library(scales)

# ---- Locate the supplementary workbook ----
candidate_files <- c(
  "data/Supplementary_Data_File_1.xlsx",
  "../data/Supplementary_Data_File_1.xlsx",
  "Supplementary_Data_File_1.xlsx"
)

existing_files <- candidate_files[file.exists(candidate_files)]

if (length(existing_files) == 0) {
  stop(
    "Supplementary_Data_File_1.xlsx was not found. Run the script from the repository root or the code directory."
  )
}

excel_file <- existing_files[1]

message("Using workbook: ", excel_file)

output_dir <- "Narrative_Figures"
dir.create(output_dir, showWarnings = FALSE)

# ---- Read final article-level sheets ----
studies <- read_excel(excel_file, sheet = "Included_Study_Characteristics")
performance <- read_excel(excel_file, sheet = "Reported_SVM_Performance")
rob <- read_excel(excel_file, sheet = "Risk_of_Bias")

# Basic checks
required_study_columns <- c(
  "Study_ID", "Publication_Year", "Data_Family", "Clinical_Task",
  "Validation_Category", "Omics_Layer"
)
required_perf_columns <- c("Study_ID", "Reported_Metric")
required_rob_columns <- c(
  "Study_ID", "Participants", "Predictors", "Outcome", "Analysis",
  "Overall_Judgment"
)

stopifnot(all(required_study_columns %in% names(studies)))
stopifnot(all(required_perf_columns %in% names(performance)))
stopifnot(all(required_rob_columns %in% names(rob)))

# ---- Theme ----
theme_review <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, size = 8)
  )

save_figure <- function(plot_object, filename, width, height) {
  ggsave(
    file.path(output_dir, paste0(filename, ".png")),
    plot_object, width = width, height = height, dpi = 600, bg = "white"
  )
  ggsave(
    file.path(output_dir, paste0(filename, ".tiff")),
    plot_object, width = width, height = height, dpi = 600,
    compression = "lzw", bg = "white"
  )
}

# ================================================================
# Figure 2. Descriptive distribution of studies
# ================================================================
data_family_counts <- studies %>%
  count(Data_Family, name = "Studies") %>%
  mutate(Data_Family = fct_reorder(Data_Family, Studies))

clinical_task_counts <- studies %>%
  count(Clinical_Task, name = "Studies") %>%
  mutate(Clinical_Task = fct_reorder(Clinical_Task, Studies))

p2a <- ggplot(data_family_counts, aes(x = Studies, y = Data_Family)) +
  geom_col() +
  geom_text(aes(label = Studies), hjust = -0.15, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "A. Data family",
    x = "Number of included studies",
    y = NULL
  ) +
  theme_review

p2b <- ggplot(clinical_task_counts, aes(x = Studies, y = Clinical_Task)) +
  geom_col() +
  geom_text(aes(label = Studies), hjust = -0.15, size = 3.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "B. Clinical application reported by the source article",
    x = "Number of included studies",
    y = NULL
  ) +
  theme_review +
  theme(axis.text.y = element_text(size = 8.5))

figure2 <- p2a / p2b +
  plot_annotation(
    title = "Descriptive profile of the included studies",
    subtitle = "Counts summarize article-level characteristics; no performance comparison or statistical testing was performed.",
    tag_levels = NULL
  )

save_figure(figure2, "Figure2_Study_Characteristics", 10, 11)

# ================================================================
# Figure 3. Validation categories by data family
# ================================================================
validation_counts <- studies %>%
  count(Data_Family, Validation_Category, name = "Studies")

figure3 <- ggplot(
  validation_counts,
  aes(x = fct_reorder(Data_Family, Studies, .fun = sum), y = Studies, fill = Validation_Category)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Validation approaches reported across data families",
    subtitle = "The figure presents descriptive study counts only.",
    x = NULL,
    y = "Number of included studies",
    fill = "Validation category",
    caption = "Internal only, internal and external, external only, no validation, and unclear were retained as recorded in the extraction workbook."
  ) +
  theme_review +
  theme(legend.position = "bottom")

save_figure(figure3, "Figure3_Validation_Design", 10, 6.5)

# ================================================================
# Figure 4. PROBAST summary (descriptive)
# ================================================================
rob_long <- rob %>%
  select(Study_ID, Participants, Predictors, Outcome, Analysis, Overall_Judgment) %>%
  pivot_longer(
    cols = -Study_ID,
    names_to = "Domain",
    values_to = "Judgment"
  ) %>%
  mutate(
    Domain = recode(Domain, Overall_Judgment = "Overall"),
    Domain = factor(
      Domain,
      levels = c("Participants", "Predictors", "Outcome", "Analysis", "Overall")
    ),
    Judgment = factor(Judgment, levels = c("Low", "Unclear", "High"))
  )

rob_summary <- rob_long %>%
  count(Domain, Judgment, name = "Studies") %>%
  group_by(Domain) %>%
  mutate(Percent = Studies / sum(Studies)) %>%
  ungroup()

figure4 <- ggplot(rob_summary, aes(x = Domain, y = Percent, fill = Judgment)) +
  geom_col(width = 0.72) +
  geom_text(
    aes(label = ifelse(Studies > 0, paste0(Studies, " (", percent(Percent, accuracy = 1), ")"), "")),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
  labs(
    title = "PROBAST risk-of-bias judgments",
    subtitle = "Study-level methodological appraisal of the 75 included studies.",
    x = NULL,
    y = "Percentage of included studies",
    fill = "Judgment",
    caption = "PROBAST judgments are presented descriptively; no statistical comparison was performed."
  ) +
  theme_review +
  theme(legend.position = "bottom")

save_figure(figure4, "Figure4_PROBAST_Summary", 10, 6.5)

# ================================================================
# Supplementary Figure S1. Study-level PROBAST traffic-light plot
# ================================================================
rob_order <- studies %>%
  select(Study_ID, Publication_Year) %>%
  left_join(rob %>% select(Study_ID, Overall_Judgment), by = "Study_ID") %>%
  arrange(Overall_Judgment, Publication_Year, Study_ID) %>%
  pull(Study_ID)

rob_traffic <- rob_long %>%
  mutate(
    Study_ID = factor(Study_ID, levels = rev(rob_order)),
    Domain = factor(
      Domain,
      levels = c("Participants", "Predictors", "Outcome", "Analysis", "Overall")
    )
  )

figure_s1 <- ggplot(rob_traffic, aes(x = Domain, y = Study_ID, fill = Judgment)) +
  geom_tile(width = 0.9, height = 0.9) +
  labs(
    title = "Study-level PROBAST judgments",
    x = NULL,
    y = "Study ID",
    fill = "Judgment",
    caption = "Each tile represents the judgment recorded for one study and one PROBAST domain."
  ) +
  theme_review +
  theme(
    axis.text.y = element_text(size = 5.3),
    legend.position = "bottom",
    panel.grid = element_blank()
  )

save_figure(figure_s1, "FigureS1_PROBAST_Traffic_Light", 8.5, 15)

# ================================================================
# Supplementary Figure S2. Availability of reported SVM metrics
# ================================================================
metric_presence <- performance %>%
  mutate(
    Metric_Group = case_when(
      str_to_lower(Reported_Metric) == "auc" ~ "AUC",
      str_detect(str_to_lower(Reported_Metric), "accuracy") ~ "Accuracy",
      str_to_lower(Reported_Metric) == "sensitivity" ~ "Sensitivity",
      str_to_lower(Reported_Metric) == "specificity" ~ "Specificity",
      str_to_lower(Reported_Metric) %in% c("f1", "f1 score", "f1-score") ~ "F1 score",
      str_to_lower(Reported_Metric) %in% c("ppv", "precision") ~ "PPV / precision",
      str_to_lower(Reported_Metric) == "npv" ~ "NPV",
      TRUE ~ "Other reported metric"
    )
  ) %>%
  distinct(Study_ID, Metric_Group) %>%
  mutate(Reported = "Reported") %>%
  complete(
    Study_ID = unique(studies$Study_ID),
    Metric_Group = c(
      "AUC", "Accuracy", "Sensitivity", "Specificity",
      "F1 score", "PPV / precision", "NPV", "Other reported metric"
    ),
    fill = list(Reported = "Not reported")
  )

study_order <- studies %>%
  arrange(Publication_Year, Study_ID) %>%
  pull(Study_ID)

metric_presence <- metric_presence %>%
  mutate(
    Study_ID = factor(Study_ID, levels = rev(study_order)),
    Metric_Group = factor(
      Metric_Group,
      levels = c(
        "AUC", "Accuracy", "Sensitivity", "Specificity",
        "F1 score", "PPV / precision", "NPV", "Other reported metric"
      )
    )
  )

figure_s2 <- ggplot(metric_presence, aes(x = Metric_Group, y = Study_ID, fill = Reported)) +
  geom_tile(width = 0.9, height = 0.9) +
  labs(
    title = "Availability of SVM performance measures reported by each study",
    x = NULL,
    y = "Study ID",
    fill = NULL,
    caption = "A filled tile indicates that the source article reported at least one value for that metric category. Values were not pooled or transformed."
  ) +
  theme_review +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    axis.text.y = element_text(size = 5.3),
    legend.position = "bottom",
    panel.grid = element_blank()
  )

save_figure(figure_s2, "FigureS2_Reported_Metric_Availability", 10, 15)

# ---- Export descriptive tables used for the main figures ----
write.csv(data_family_counts, file.path(output_dir, "Figure2A_Data_Family_Counts.csv"), row.names = FALSE)
write.csv(clinical_task_counts, file.path(output_dir, "Figure2B_Clinical_Task_Counts.csv"), row.names = FALSE)
write.csv(validation_counts, file.path(output_dir, "Figure3_Validation_Counts.csv"), row.names = FALSE)
write.csv(rob_summary, file.path(output_dir, "Figure4_PROBAST_Counts.csv"), row.names = FALSE)

message("Completed. Figures were saved in: ", normalizePath(output_dir))
message("No meta-analysis or inferential analysis was performed.")
