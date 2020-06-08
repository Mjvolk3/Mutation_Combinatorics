#Date: "2020-06-02"
#Author: Michael Volk

pipette_error <- function(PRECOG_file = NULL, error_row = c(), error_col = c()) {
        
#original arguments
# PRECOG_file = "BioscreenC/experiments/ex12/Mv_ex12_single_mutants.tsv"
# error_row <- c(4,5,6,7,7,6)
# error_col <- c(7,7,7,7,11,19)
        
        library(filesstrings)
        error_pip <- data.frame(error_row, error_col)
        new_file_name <- paste0(str_split(PRECOG_file, ".tsv")[[1]][1],"_pipette_error.csv")
        write.csv(error_pip, new_file_name)
        file.move(new_file_name, paste0("BioscreenC/experiments/",str_extract(PRECOG_file, "(ex[0-9]+)")))
}