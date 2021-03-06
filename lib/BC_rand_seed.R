#Date: 2020.03.07
#Author: Michael Volk 

# sample_list_file <- ('BioscreenC/experiments/ex12/Mv_ex12_sample_list.csv')
# sample_list_file is a sample list
BC_rand_seed <- function(sample_list_file = NULL){
        
        exp_num <- str_extract(str_extract(sample_list_file, "(ex[0-9]+)"), "[0-9]+")
        dir.create(sprintf("./position_list/ex%s", exp_num))
        exp_list <- read.csv(sample_list_file, colClasses = 'character')
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
                id <- sprintf("./position_list/ex%s/%s.csv", exp_num, i)
                print(id)
                write.csv(n,id)
        }
}


