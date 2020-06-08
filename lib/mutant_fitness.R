#Date: "2020-06-03"
#Author: Michael Volk

mutant_fitness <- function(growth_data){
                
        #source('C:/Users/michaelvolk/Desktop/Mutation_Combinatorics/lib/process_PRECOG.R')
        #arguments
        #growth_data <- process_PRECOG("BioscreenC/experiments/ex12/Mv_ex12_single_mutants.tsv")

        #sample factor done automatic plot coloring
        sample_fac <- data.frame(growth_data$sample)
        colnames(sample_fac) <- "sample"
        sample_fac <- mutate(sample_fac, label = str_extract(sample, "[^br]+"))
        sample_fac <- mutate(sample_fac, label = gsub('.{1}$', '', label))
        growth_data <- suppressWarnings(left_join(sample_fac, growth_data, by = "sample"))
        
        #change char's to factors for plotting. This is necessary for ordering the x-axis according the the sorted table.       
        growth_data$sample <- sapply(growth_data$sample, as.factor)
        growth_data$label <- sapply(growth_data$label, as.factor)
        
        #Calculating fitness with normalization by BY_WT with minimal error bound
        growth_data_WT <- growth_data[growth_data$label == "BY_WT",]
        WT_min_err_mu <- growth_data_WT[growth_data_WT$doubling_time_sd == min(growth_data_WT$doubling_time_sd),]$doubling_time_mu
        WT_min_err_sd <- growth_data_WT[growth_data_WT$doubling_time_sd == min(growth_data_WT$doubling_time_sd),]$doubling_time_sd
        WT_min_err_se <- growth_data_WT[growth_data_WT$doubling_time_sd == min(growth_data_WT$doubling_time_sd),]$doubling_time_se
        WT_min_err_ci <- growth_data_WT[growth_data_WT$doubling_time_sd == min(growth_data_WT$doubling_time_sd),]$doubling_time_ci
        
        fitness_data <- mutate(growth_data, 
                               fitness_mu = doubling_time_mu/WT_min_err_mu,
                               fitness_sd = sqrt((doubling_time_sd/doubling_time_mu)^2 + (WT_min_err_sd/WT_min_err_mu)^2),
                               fitness_se = sqrt((doubling_time_sd/doubling_time_mu)^2 + (WT_min_err_se/WT_min_err_mu)^2),
                               fitness_ci = sqrt((doubling_time_ci/doubling_time_mu)^2 + (WT_min_err_ci/WT_min_err_mu)^2))
        return(fitness_data)
}