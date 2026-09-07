# ============================================================
# 25) Data Initialization Pipeline.R
# Purpose: Master pipeline tying the modular scripts together.
# Input:
# Output:
# ============================================================

# 01) LOAD SCRIPTS:
source("scripts/01) Raw File Index.R")
source("scripts/02) Import Data (CSV).R")
source("scripts/03) Import Data (Delimited).R")
source("scripts/04) Import Data (DAT).R")
source("scripts/05) Import Data (SHP).R")
source("scripts/06) Import Data (XLSX).R")
source("scripts/07) Standardize Column Names.R")
source("scripts/08) Clean Missing Values.R")
source("scripts/09) Clean Strings.R")
source("scripts/10) Combine Raw Tables.R")
source("scripts/11) Schema Inventory and Column Audit.R")
source("scripts/12) Duplicate Audit.R")
source("scripts/13) Data Type Guessing.R")
source("scripts/14) Data Type Conversion.R")
source("scripts/15) Missingness Summary Tables.R")
source("scripts/16) Plot Missingness Map.R")
source("scripts/17) Plot Missingness Heatmap.R")
source("scripts/18) Pairwise MIssingness Co-occurrence.R")
source("scripts/19) Initial Diagnostics (Numeric).R")
source("scripts/20) Initial Diagnostics (Categorical).R")
source("scripts/21) Initial Diagnostics (Date-Time).R")
source("scripts/22) Initial Diagnostics (SHP).R")
source("scripts/23) Initial Diagnostics (MICE).R")
source("scripts/24) Export Cleaned Data.R")


csv_raw_list <- read_many_csvs_all_character(
  folder = "data/raw",
  recursive = TRUE,
  clean_names = TRUE
)

csv_cleaned_list <-
  clean_common_missing_values_in_list(csv_raw_list)

csv_cleaned_list <-
  map(csv_cleaned_list, clean_character_strings)

schema_comparison <-
  compare_table_schemas(csv_cleaned_list)

write_csv(schema_comparison, "logs/schema_comparison_before_binding.csv")

combined_raw <-
  bind_raw_tables(csv_cleaned_list)

saveRDS(combined_raw, "data/intermediate/combined_raw_character_cleaned.rds")

dataset_audit <-
  dataset_inventory(combined_raw)

schema_audit <-
  schema_inventory(combined_raw)

# type_report <-
type_guessing_report(combined_raw)

missing_variable_table <-
  missingness_by_variable(combined_raw)

missing_pattern_table <-
  summarize_md_patterns(combined_raw)

write_csv(
  dataset_audit,
  "logs/dataset_audit.csv"
)

write_csv(
  schema_audit,
  "data/dictionary/schema_inventory.csv"
)

write_csv(
  type_report,
  "data/dictionary/type_guessing_report.csv"
)

write_csv(
  missing_variable_table,
  "outputs/tables/missingness_by_variable.csv"
)

write_csv(
  missing_pattern_table,
  "outputs/tables/missingness_patterns.csv"
)

missingness_map_plot <-
  plot_missingness_map(combined_raw)

ggsave(
  filename = "outputs/plots/missingness_map.png",
  plot = missingness_map_plot,
  width = 12,
  height = 8
)

md_pattern_plot <-
  plot_md_pattern_ggplot(combined_raw)

ggsave(
  filename = "outputs/plots/md_pattern_ggplot.png",
  plot = md_pattern_plot,
  width = 12,
  height = 8
)

message(
  "Initial data pipeline complete. Review type_guessing_report.csv before controlled type conversion."
)
