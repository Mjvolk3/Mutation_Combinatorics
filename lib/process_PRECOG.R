#Date: "2020-06-02"
#Author: Michael Volk

process_PRECOG <- function(PRECOG_file,
                           plate_rotation = NA,
                           percent_confidence = 0.90){
        
        # # original arguments
        # PRECOG_file <- 'BioscreenC/experiments/ex12/Mv_ex12_single_mutants.tsv'
        # plate_rotation <- "L"
        # percent_confidence = 0.90
        
        #libraries
        library(dplyr)
        
        #read PRECOG data
        df <- read.table(PRECOG_file, sep = '\t', header = TRUE)
        
        plate_L <- df['GT'][1:100,1]
        plate_R <- df['GT'][101:200,1]
        
        #put in matrix form
        plate_L <- matrix(plate_L, nrow = 10, ncol = 10)
        plate_R <- matrix(plate_R, nrow = 10, ncol = 10)
        
        #rotate 180 degrees if plate was improperly rotated before pipetting
        rot_180 <- function (x){
                x[] <- rev(x)
                return(x)
        }
        
        for (i in 1:length(plate_rotation)){
                if (plate_rotation[i] %in% c("l","left","L","Left","LEFT")){
                        plate_L <- rot_180(plate_L)
                } else if (plate_rotation[i] %in% c("r","right","R","Right","RIGHT")){
                        plate_R <- rot_180(plate_R)
                }
        }
        
        plate_comb <- cbind(plate_L,plate_R)
        
        #treated pipette_error
        pipette_error_file <- paste0("BioscreenC/experiments/", str_extract(PRECOG_file, "(ex[0-9]+)"),"/",
                                     str_split(rev(str_split(PRECOG_file, "/"))[[1]][4], ".tsv")[[1]][1],"_pipette_error.csv") 
        if (file.exists(pipette_error_file)){
                error_pip <- read.csv(pipette_error_file)
        } else{
                error_pip <- data.frame()
        }
        
        directory <- paste0("position_list/",str_extract(PRECOG_file, "(ex[0-9]+)"))
        sample_list_file <- paste0(str_split(PRECOG_file, "/")[[1]][1], "/", str_split(PRECOG_file, "/")[[1]][2],
                                   "/", str_split(PRECOG_file, "/")[[1]][3], "/", str_split(str_split(PRECOG_file, "/")[[1]][4], "_")[[1]][1],
                                   "_", str_split(str_split(PRECOG_file, "/")[[1]][4], "_")[[1]][[2]], "_sample_list.csv")
        sample_list <- read.csv(sample_list_file)
        growth_data <- c()
        sample_GT <- c()
        
        for (i in (1:nrow(sample_list))){
                sample <- as.character(sample_list[i,1])
                sample_pos_list <- read.csv(sprintf("%s/%s.csv",directory,sample))
                
                for (j in (1:nrow(sample_pos_list))){
                        row_pos <- as.numeric(sample_pos_list['row'][j,1])
                        col_pos <- as.numeric(sample_pos_list['col'][j,1])
                        GT <- (plate_comb[sample_pos_list['row'][j,1], sample_pos_list['col'][j,1]])
                        #only correct for error pip if they exists
                        
                        if (nrow(error_pip)!= 0){
                                #set mistaken pipette to na        
                                for (k in (1:nrow(error_pip))){
                                        if ((error_pip['error_row'][k,] == row_pos) && (error_pip['error_col'][k,] == col_pos)){
                                                GT <- NA
                                        }
                                }
                        }
                        sample_GT[j] <- GT
                }
                sample_GT_clean <- sample_GT[!is.na(sample_GT)]
                doubling_time_mu <- mean(sample_GT_clean)
                doubling_time_sd <- sd(sample_GT_clean)
                doubling_time_se <- sd(sample_GT_clean) / sqrt(length(sample_GT_clean))
                #Confidence Interval - t-test since we are working with a sample sd, not an exact sd
                #percent_confidence <- 0.90
                alpha <- (1-percent_confidence)/2
                t <- qt((1-alpha), length(sample_GT_clean)-1) # tends to 1.96 if sample size is big enough
                doubling_time_ci <- t*doubling_time_se
                
                growth_data <- rbind(growth_data, data.frame(sample, 
                                                             doubling_time_mu, 
                                                             doubling_time_sd, 
                                                             doubling_time_se, 
                                                             doubling_time_ci))
                #reset sample GT
                sample_GT <- c()
        }
        growth_data$sample <- sapply(growth_data$sample, as.character)
        
        library(stringr)
        guide_num <- function(sample = NULL){
                #function to extract guide number from sample column
                as.numeric(str_extract(str_extract(sample, "(g[0-9]+)"), "[0-9]+"))
        }
        
        growth_data <- suppressWarnings(arrange(growth_data, 
                                                str_extract(sample, "BY"), 
                                                as.numeric(str_extract(sample,"[^g]+")),
                                                guide_num(sample)))
        
        experiment_dir <- paste0(str_split(PRECOG_file, "/")[[1]][1],"/",
                                 str_split(PRECOG_file, "/")[[1]][2],"/",
                                 str_split(PRECOG_file, "/")[[1]][3])
        dir_data <- list(c(experiment_dir, growth_data))
        return(dir_data)
}