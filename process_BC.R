#Date: 2020.03.10
#Author: Michael Volk 

file_name <- 'BioscreenC/Mv_ex12_single_mutants_.tsv' 
        
library(dplyr)

df <- read.table(file_name, sep = '\t', header = TRUE)

#add conditional to function for plate roation, errors, etc.

#set NAN of table to 0.... avoiding divide out 0 for now

#df[is.na(df)] <- 0

plate_L <- df['GT'][1:100,1]
plate_R <- df['GT'][101:200,1]

#put in matrix form
plate_L <- matrix(plate_L, nrow = 10, ncol = 10)
plate_R <- matrix(plate_R, nrow = 10, ncol = 10)

#rotate 180 degrees if plate was rotated before pipetting
rot_180 <- function (x){
        x[] <- rev(x)
        return(x)
}

#only rotate plate_L as this was the plate with the mistake
plate_L <- rot_180(plate_L)

plate_comb <- cbind(plate_L,plate_R)

## HERE

#miss pip


sample_list <- read.csv('BioscreenC_ex1.csv')
directory <- ('position_list')

growth_data <- c()
sample_GT <- c()
#delete
test <- c()

row <- c(4,5,6,7,7,6)
col <- c(7,7,7,7,11,19)
mis_pip <- data.frame(row,col)
count <- 0

for (i in (1:nrow(sample_list))){
        sample <- as.character(sample_list[i,1])
        sample_pos_list <- read.csv(sprintf("%s/%s.csv",directory,sample))
       
        for (j in (1:nrow(sample_pos_list))){
                row_pos <- as.numeric(sample_pos_list['row'][j,1])
                col_pos <- as.numeric(sample_pos_list['col'][j,1])
                
                GT <- (plate_comb[sample_pos_list['row'][j,1], sample_pos_list['col'][j,1]])
                
                
                #set mistaken pipette to na
                for (k in (1:nrow(mis_pip))){
                        if ((mis_pip['row'][k,] == row_pos) && (mis_pip['col'][k,] == col_pos)){
                                GT <- NA
                        }
                }
                sample_GT[j] <- GT
        }
        sample_GT_clean <- sample_GT[!is.na(sample_GT)]
        test[i] <- length(sample_GT_clean)
        
        doubling_time_mu <- mean(sample_GT_clean)
        doubling_time_sd <- sd(sample_GT_clean)
        doubling_time_se <- sd(sample_GT_clean) / sqrt(length(sample_GT_clean))
        #Confidence Interval - t-test bc we are working with a sample sd, not an exact sd
        percent_confidence <- 0.90
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

print(sum(test))

#sample factor done for automatic plot coloring
sample_fac <- data.frame(growth_data$sample)
colnames(sample_fac) <- "sample"
sample_fac <- mutate(sample_fac, label = str_extract(sample, "[^br]+"))
sample_fac <- mutate(sample_fac, label = gsub('.{1}$', '', label))
growth_data <- suppressWarnings(left_join(sample_fac, growth_data, by = "sample"))

#change char's to factors for plotting. This is necessary for ordering the x-axis according the the sorted table
growth_data$sample <- sapply(growth_data$sample, as.factor)
growth_data$label <- sapply(growth_data$label, as.factor)

#adding noramlized by BY_WT with minimal error bound
growth_data_WT <- growth_data[growth_data$label == "BY_WT",]
WT_min_err_mu <- growth_data_WT[growth_data_WT$doubling_time_sd == min(growth_data_WT$doubling_time_sd),]$doubling_time_mu
WT_min_err_sd <- growth_data_WT[growth_data_WT$doubling_time_sd == min(growth_data_WT$doubling_time_sd),]$doubling_time_sd
WT_min_err_se <- growth_data_WT[growth_data_WT$doubling_time_sd == min(growth_data_WT$doubling_time_sd),]$doubling_time_se
WT_min_err_ci <- growth_data_WT[growth_data_WT$doubling_time_sd == min(growth_data_WT$doubling_time_sd),]$doubling_time_ci

growth_data <- mutate(growth_data, 
                      doubling_time_mu_normalized = doubling_time_mu/WT_min_err_mu,
                      doubling_time_sd_normalized = sqrt((doubling_time_sd/doubling_time_mu)^2 + (WT_min_err_sd/WT_min_err_mu)^2),
                      doubling_time_se_normalized = sqrt((doubling_time_sd/doubling_time_mu)^2 + (WT_min_err_se/WT_min_err_mu)^2),
                      doubling_time_ci_normalized = sqrt((doubling_time_ci/doubling_time_mu)^2 + (WT_min_err_ci/WT_min_err_mu)^2))
                      
#Plotting
library(ggplot2)
doubling_time_plotted <- growth_data$doubling_time_mu_normalized
error_bar_plotted <- growth_data$doubling_time_sd_normalized

g1 <- ggplot(growth_data, aes(x = sample, y = doubling_time_plotted, fill = label)) + geom_point() +
        geom_bar(stat = "identity", position =  position_dodge(),  width = 0.75, linetype = 1, colour = "black") +
        ggtitle(label = "Sinlge Mutant Doubling Times",
                subtitle = "Validating Growth Assay") +
        xlab("Sample") +
        ylab("Doubling Time (hours)") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              axis.text.x = element_text(angle=45, hjust=1))+
        scale_fill_brewer(palette="RdYlBu") +  
        geom_errorbar(aes(x = sample, 
                          ymin= doubling_time_plotted - error_bar_plotted, 
                          ymax= doubling_time_plotted + error_bar_plotted), width=0.25, colour="black", size=0.75)
g1

#Importing previously published data
library(readxl)
single_ko_published_file <- "./published_data/10.1038-nmeth.1534_S1_Single-mutant_fitness_standard.xls"
experiement_translation_file <- "gene_names_from_experiments.xlsx"
single_ko_published_tbl <- read_excel(single_ko_published_file, col_names = c("Systematic_Name","SGA_fitness","sd"))
experiement_translation_tbl <- read_excel(experiement_translation_file)

ID_list <- suppressWarnings(unique(na.omit(as.numeric(str_extract(unique(growth_data$label),"[^g]+")))))

growth_data_systematic_names <- c()
test <- list()

for (i in ID_list){
        if (i %in% experiement_translation_tbl$ID){
                growth_data_systematic_names <- suppressWarnings(c(growth_data_systematic_names, 
                                                      experiement_translation_tbl[experiement_translation_tbl$ID == i,]$Systematic_Name)
                )
        }
}

growth_data_pub <- list()
for (i in 1:length(growth_data_systematic_names)){
        if (growth_data_systematic_names[i] %in% single_ko_published_tbl$Systematic_Name){
                dat <- single_ko_published_tbl[single_ko_published_tbl$Systematic_Name == growth_data_systematic_names[i],]
                #dat$i <- i; for tracking indexing
                growth_data_pub[[i]] <- dat
        }
}
growth_data_pub_tbl <- dplyr::bind_rows(growth_data_pub)
colnames(growth_data_pub_tbl)[2] <- "fitness"
colnames(growth_data_pub_tbl)[3] <- "error"
growth_data_pub_tbl_match <- growth_data_pub_tbl %>%  mutate(Systematic_Sample = as.factor(paste0(Systematic_Name, "_Pub"))) %>% 
        select(Systematic_Sample, Systematic_Name, fitness, error)
growth_data_pub_tbl_match$Systematic_Name <- sapply(growth_data_pub_tbl_match$Systematic_Name, as.factor) 


#preparing my data for merge with systematic names
growth_data_simp <- growth_data %>% select(sample, label, doubling_time_mu_normalized, doubling_time_ci_normalized)
growth_data_sys_fac <- list()
growth_data_sys_lab <- list()
for (i in 1:nrow(growth_data_simp)){
        id <- str_extract(growth_data_simp$label,"[^g]+")
        samp <- as.character(growth_data_simp$sample)[i]
        if (as.numeric(id[i]) %in% experiement_translation_tbl$ID){
                #guide <-  paste0("_g", as.numeric(str_extract(samp, "(?<=g)(.*?)(?=_)")))
                guide_br <- paste0("_g", str_extract(samp, "(?<=g).*"))
                sys_name <- experiement_translation_tbl[experiement_translation_tbl$ID == id[i],]$Systematic_Name
                sys_guide <- paste0(sys_name, guide_br)
                growth_data_sys_lab <- c(growth_data_sys_lab, sys_name)
                growth_data_sys_fac <- c(growth_data_sys_fac,sys_guide)
        }
        else {
                
                growth_data_sys_lab <- c(growth_data_sys_lab, as.character(growth_data_simp$label[1]))
                growth_data_sys_fac <- c(growth_data_sys_fac, samp)
        }
}

growth_data_simp <- growth_data_simp %>% 
        mutate(Systematic_Sample = as.factor(as.character(growth_data_sys_fac))) %>% 
        mutate(Systematic_Name = as.factor(as.character(growth_data_sys_lab)))
        
growth_data_match <- growth_data_simp %>% select(Systematic_Sample, Systematic_Name, doubling_time_mu_normalized, doubling_time_ci_normalized)
colnames(growth_data_match)[3] <- "fitness"
colnames(growth_data_match)[4] <- "error"

#merger
growth_data_comb <- tibble(bind_rows(growth_data_match, growth_data_pub_tbl_match))
growth_data_comb$Systematic_Name <- sapply(growth_data_comb$Systematic_Name, as.factor)

#Reorganize factors
growth_data_comb <- growth_data_comb %>% arrange(Systematic_Name)
growth_data_comb$Systematic_Sample <- factor(growth_data_comb$Systematic_Sample, 
                                             levels = growth_data_comb$Systematic_Sample)
#levels(growth_data_comb$Systematic_Sample) <- factor(growth_data_comb$Systematic_Sample)
#Plotting
doubling_time_plotted <- growth_data$doubling_time_mu_normalized
error_bar_plotted <- growth_data$doubling_time_sd_normalized

g2 <- ggplot(growth_data_comb, aes(x = Systematic_Sample, y = fitness, fill = Systematic_Name)) +
        geom_bar(stat = "identity", position =  position_dodge(),  width = 0.6, linetype = 1, colour = "gray20", alpha = 0.9, size = 0.15) +
        ggtitle(label = "Single Mutant Fitness",
                subtitle = "Validating Growth Assay") +
        xlab("Sample") +
        ylab("Fitness") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              axis.text.x = element_text(angle=45, hjust=1))+
        scale_fill_brewer(palette="RdYlBu") +  
        geom_errorbar(aes(x = Systematic_Sample, 
                          ymin= fitness - error, 
                          ymax= fitness + error), width=0.20, colour="black", size=0.25)
g2

ggsave("g2.pdf")


#zoom








