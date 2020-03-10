#Date: 2020.03.10
#Author: Michael Volk 

file_current <- 'BioscreenC/Mv_ex12_single_mutants_.tsv' 
        
df <- read.table(file_current, sep = '\t', header = TRUE)

#add conditional to function for plate roation, errors, etc.

#set NAN of table to 0.... avoiding divide out 0 for now
df[is.na(df)] <- 0.000001

plate_L <- df['GT'][1:100,1]
plate_R <- df['GT'][101:200,1]

#put in matrix form
plate_L <- matrix(plate_L, nrow = 10, ncol = 10)
plate_R <- matrix(plate_R, nrow = 10, ncol = 10)

#rotate 180 degrees if plate was roated before pipetting
rot_180 <- function (x){
        x[] <- rev(x)
        #return(x)
}

#only rotate plate_L as this was the plate with the mistake
rot_180(plate_L)

plate_comb <- cbind(plate_L,plate_R)


## HERE

sample_list <- read.csv('BioscreenC_ex1.csv')
directory <- ('position_list')

growth_data <- c()
sample_GT <- c()


for (i in (1:nrow(sample_list))){
        sample <- as.character(sample_list[i,1])
        sample_pos <- read.csv(sprintf("%s/%s.csv",directory,sample))
       
        for (j in (1:nrow(sample_pos))){
                sample_GT[j] <- (plate_comb[sample_pos['row'][j,1], sample_pos['col'][j,1]])
        }
        
        DT_mu <- mean(sample_GT)
        DT_sigma <- sd(sample_GT)
        growth_data <- rbind(growth_data, data.frame(sample,DT_mu,DT_sigma))
}

new_data <- growth_data[order(growth_data$mu),]


