parseText <- function(text) {
  input_text <- gsub(";", "|", text)  # Replace delimiters with pipe
  input_text <- gsub(" ", "|", input_text)  # Replace delimiters with pipe
  input_text <- gsub(",", ".", input_text)  # Replace commas for spanish formats
  data_array <- as.numeric(unlist(strsplit(input_text, "|", fixed = TRUE)))  # Convert to numeric array
  data_array[!is.na(data_array)]  # Remove NA values
}