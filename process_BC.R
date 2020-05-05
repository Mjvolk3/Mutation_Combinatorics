#Date: 2020.03.10
#Author: Michael Volk 

file_current <- 'BioscreenC/Mv_ex12_single_mutants_.tsv' 
        
df <- read.table(file_current, sep = '\t', header = TRUE)

#add conditional to function for plate roation, errors, etc.

#set NAN of table to 0.... avoiding divide out 0 for now

#df[is.na(df)] <- 0

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
        
        DT_mu <- mean(sample_GT_clean)
        DT_sigma <- sd(sample_GT_clean)
        growth_data <- rbind(growth_data, data.frame(sample,DT_mu,DT_sigma))
       
         #reset sample GT
        sample_GT <- c()
}

print(sum(test))
