#Date: "2020-06-03"
#Author: Michael Volk

plot_compare_fitness <- function(fitness_compare_data,
                        
                                 title = "Single Mutant Fitness",
                                 subtitle = "Validating Growth Assay"){
        
# source('C:/Users/michaelvolk/Desktop/Mutation_Combinatorics/lib/process_PRECOG.R')
# growth_data <- process_PRECOG("BioscreenC/experiments/ex12/Mv_ex12_single_mutants.tsv")
# source('C:/Users/michaelvolk/Desktop/Mutation_Combinatorics/lib/mutant_fitness.R')
# fitness_data <- mutant_fitness(growth_data)
# source('C:/Users/michaelvolk/Desktop/Mutation_Combinatorics/lib/mutant_fitness_compare.R')
# arguments
# fitness_compare_data <- mutant_fitness_compare(fitness_data)

g <- ggplot(fitness_compare_data, aes(x = Systematic_Sample, y = fitness, fill = Systematic_Name)) +
        geom_bar(stat = "identity", position =  position_dodge(),  width = 0.6, linetype = 1, colour = "gray20", alpha = 0.9, size = 0.15) +
        ggtitle(label = "Single Mutant Fitness",
                subtitle = "Validating Growth Assay") +
        xlab("Sample") +
        ylab(Fitness~Score~(DT["Sample"]/DT["WT"])) +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              axis.text.x = element_text(angle=45, hjust=1))+
        scale_fill_brewer(palette="RdYlBu") +  
        geom_errorbar(aes(x = Systematic_Sample, 
                          ymin= fitness - error, 
                          ymax= fitness + error), width=0.20, colour="black", size=0.25)


ggsave("ex12_plot_compare_fitness.pdf")       

}

