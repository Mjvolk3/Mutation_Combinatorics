#Date: 2020.03.07
#Author: Michael Volk 


directory <- 'BioscreenC_ex1.csv'
exp_list <- read.csv(directory, colClasses = 'character' )

max_tech_rep <- 200 %% nrow(exp_list)
num_WT_assign <- 200 - max_tech_rep

exp_list_comb <- exp_list[,1]

for (i in 1:(max_tech_rep-1)) {
        exp_list_comb <- c(exp_list_comb,exp_list[,1])
}
 
while (length(exp_list_comb) < 200) {
        WT_char_sub <- substr(as.character(exp_list[1,1]), 1,5)
        for (val in exp_list[,1]){
                
                current_sample <- substr(val,1,5)
                
                if (current_sample == (WT_char_sub) && length(exp_list_comb) != 200){
                        exp_list_comb <- c(exp_list_comb,as.character(val))
                }
        }
}

m <- matrix(sample(exp_list_comb), nrow = 10, ncol = 20)

for (i in exp_list[,1]){
        n <- which(m == i, arr.ind = TRUE)
        n <- data.frame(n, row.name = i)
        #commented until functionalized
        #id <- sprintf("C:/Users/michaelvolk/Desktop/Mutation_Combinatorics/position_list/%s.csv",i)
        print(id)
        write.csv(n,id)
}



