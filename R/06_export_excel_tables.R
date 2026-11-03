# Create an Excel workbook with the reported result tables.
#
# The CSV files in the active results folder remain the primary
# machine-readable tables.
# This workbook is an additional convenience file for opening the results in
# Excel without locale-specific CSV import problems.

source(file.path("R", "00_setup.R"))

write_results_workbook(
  table_folder = paths$tables,
  workbook_path = file.path(paths$tables, "results_tables.xlsx")
)
