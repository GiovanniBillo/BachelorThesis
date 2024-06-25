# Load necessary library
library(lubridate)

# Create directories if they do not exist
dir.create("code", showWarnings = FALSE)
dir.create("images", showWarnings = FALSE)
dir.create("latex", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

# Get the current date
current_date <- Sys.Date()

# Function to delete files older than one month
delete_old_files <- function(pattern) {
  files <- list.files(pattern = pattern)
  file_info <- file.info(files)
  old_files <- files[which(difftime(current_date, file_info$mtime, units = "days") > 30)]
  file.remove(old_files)
}

# Delete old Excel, image, or HTML files
delete_old_files("\\.(xls|xlsx|csv|png|jpg|jpeg|gif|html)$")

# Get a list of all remaining files in the current directory
files <- list.files(pattern = "*")

# Move R files to the 'code' directory
r_files <- list.files(pattern = "\\.R$")
file.rename(r_files, file.path("code", r_files))

# Move image and HTML files to the 'images' directory
img_html_files <- list.files(pattern = "\\.(png|jpg|jpeg|gif|html)$")
file.rename(img_html_files, file.path("images", img_html_files))

# Move LaTeX files to the 'latex' directory
latex_files <- list.files(pattern = "\\.(tex|bib|sty)$")
file.rename(latex_files, file.path("latex", latex_files))

# Move Excel files to the 'data' directory
excel_files <- list.files(pattern = "\\.(xls|xlsx|csv)$")
file.rename(excel_files, file.path("data", excel_files))
