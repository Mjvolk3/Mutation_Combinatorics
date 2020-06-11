#Date: "2020-06-03"
#Author: Michael Volk

mutant_fitness_compare <- function(fitness_data){
        
        # source('C:/Users/michaelvolk/Desktop/Mutation_Combinatorics/lib/process_PRECOG.R')
        # growth_data <- process_PRECOG("BioscreenC/experiments/ex12/Mv_ex12_single_mutants.tsv")
        # source('C:/Users/michaelvolk/Desktop/Mutation_Combinatorics/lib/mutant_fitness.R')
        # arguments
        # fitness_data <- mutant_fitness(growth_data)
        
        #directory of published data and translation files
        single_ko_published_file <- "./published_data/10.1038-nmeth.1534_S1_Single-mutant_fitness_standard.xls"
        experiment_translation_file <- "./BioscreenC/gene_names_from_experiments.xlsx"
        
        #Importing previously published data
        library(readxl)
        
        single_ko_published_tbl <- read_excel(single_ko_published_file, col_names = c("Systematic_Name","SGA_fitness","sd"))
        experiment_translation_tbl <- read_excel(experiment_translation_file)
        
        ID_list <- suppressWarnings(unique(na.omit(as.numeric(str_extract(unique(fitness_data$label),"[^g]+")))))
        
        fitness_data_systematic_names <- c()
        test <- list()
        
        for (i in ID_list){
                if (i %in% experiment_translation_tbl$ID){
                        fitness_data_systematic_names <- suppressWarnings(c(fitness_data_systematic_names, 
                                                                            experiment_translation_tbl[experiment_translation_tbl$ID == i,]$Systematic_Name)
                        )
                }
        }
        
        fitness_data_pub <- list()
        for (i in 1:length(fitness_data_systematic_names)){
                if (fitness_data_systematic_names[i] %in% single_ko_published_tbl$Systematic_Name){
                        dat <- single_ko_published_tbl[single_ko_published_tbl$Systematic_Name == fitness_data_systematic_names[i],]
                        #dat$i <- i; for tracking indexing
                        fitness_data_pub[[i]] <- dat
                }
        }
        fitness_data_pub_tbl <- dplyr::bind_rows(fitness_data_pub)
        colnames(fitness_data_pub_tbl)[2] <- "fitness"
        colnames(fitness_data_pub_tbl)[3] <- "error"
        fitness_data_pub_tbl_match <- fitness_data_pub_tbl %>%  mutate(Systematic_Sample = as.factor(paste0(Systematic_Name, "_Pub"))) %>% 
                select(Systematic_Sample, Systematic_Name, fitness, error)
        fitness_data_pub_tbl_match$Systematic_Name <- sapply(fitness_data_pub_tbl_match$Systematic_Name, as.factor) 
        
        
        #preparing my data for merge with systematic names
        fitness_data_simp <- fitness_data %>% select(sample, label, fitness_mu, fitness_ci)
        fitness_data_sys_fac <- list()
        fitness_data_sys_lab <- list()
        for (i in 1:nrow(fitness_data_simp)){
                id <- str_extract(fitness_data_simp$label,"[^g]+")
                samp <- as.character(fitness_data_simp$sample)[i]
                if (as.numeric(id[i]) %in% experiment_translation_tbl$ID){
                        #guide <-  paste0("_g", as.numeric(str_extract(samp, "(?<=g)(.*?)(?=_)")))
                        guide_br <- paste0("_g", str_extract(samp, "(?<=g).*"))
                        sys_name <- experiment_translation_tbl[experiment_translation_tbl$ID == id[i],]$Systematic_Name
                        sys_guide <- paste0(sys_name, guide_br)
                        fitness_data_sys_lab <- c(fitness_data_sys_lab, sys_name)
                        fitness_data_sys_fac <- c(fitness_data_sys_fac,sys_guide)
                }
                else {
                        
                        fitness_data_sys_lab <- c(fitness_data_sys_lab, as.character(fitness_data_simp$label[1]))
                        fitness_data_sys_fac <- c(fitness_data_sys_fac, samp)
                }
        }
        
        fitness_data_simp <- fitness_data_simp %>% 
                mutate(Systematic_Sample = as.factor(as.character(fitness_data_sys_fac))) %>% 
                mutate(Systematic_Name = as.factor(as.character(fitness_data_sys_lab)))
        
        fitness_data_match <- fitness_data_simp %>% select(Systematic_Sample, Systematic_Name, fitness_mu, fitness_ci)
        colnames(fitness_data_match)[3] <- "fitness"
        colnames(fitness_data_match)[4] <- "error"
        
        #merger
        fitness_compare_data <- tibble(bind_rows(fitness_data_match, fitness_data_pub_tbl_match))
        fitness_compare_data$Systematic_Name <- sapply(fitness_compare_data$Systematic_Name, as.factor)
        
        #Reorganize factors
        fitness_compare_data <- fitness_compare_data %>% arrange(Systematic_Name)
        fitness_compare_data$Systematic_Sample <- factor(fitness_compare_data$Systematic_Sample, 
                                                         levels = fitness_compare_data$Systematic_Sample)
        fitness_compare_data <- fitness_compare_data %>% mutate(local_directory = fitness_data$local_directory[[1]])
        
        return(fitness_compare_data)
}