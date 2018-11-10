library(openxlsx)

mcq_data <- readRDS("./OUT/mcq_summary.RDS")

wb <- createWorkbook()

# Make worksheet for each tab
lapply(names(mcq_data), function(x) {
        sheet_name <- trimws(gsub(":", "", substr(x, 1, 3)))
        addWorksheet(wb, sheet_name)
        writeData(wb, sheet_name, x)
        })

# Write each table to a new sheet
lapply(seq_along(mcq_data), function(i) {
        writeData(wb, wb$sheet_names[i], mcq_data[[i]], startRow = 2)
        })

# Save workbook
saveWorkbook(wb, file = "./OUT/mcq_summary.xlsx")
