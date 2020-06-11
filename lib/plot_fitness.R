#Date: "2020-06-02"
#Author: Michael Volk

plot_fitness <- function(fitness_data,
                         yaxis = "fitness_mu",
                         error_bars = "fitness_ci",
                         title = "Fitness Scores",
                         subtitle = "Single Mutant Controls"){
        #original arguments
        # fitness_data <- process_PRECOG(PRECOG_file, sample_list, plate_rotation = "L", percent_confidence = 0.90)
        # yaxis <- "fitness_mu"
        # error_bars <- "fitness_ci"
        # title <- "title"
        # subtitle <- "subtitle"
        #Plotting
        
        library(ggplot2)
        #User input
        yaxis_plotted <- fitness_data[[yaxis]]
        error_bar_plotted <- fitness_data[[error_bars]]
        
        if (yaxis == "fitness_mu"){
                ylabel <- expression(Fitness~Score~(DT["Sample"]/DT["WT"]))  
        } else{
                ylabel <- "Doubling Time (hours)"
        }
        
        g <- ggplot(fitness_data, aes(x = sample, y = yaxis_plotted, fill = label)) +
                geom_bar(stat = "identity", position =  position_dodge(),  width = 0.75, linetype = 1, colour = "black") +
                ggtitle(label = title,
                        subtitle = subtitle) +
                xlab("Sample") +
                ylab(ylabel) +
                theme(plot.title = element_text(hjust = 0.5),
                      plot.subtitle = element_text(hjust = 0.5),
                      axis.text.x = element_text(angle=45, hjust=1))+
                scale_fill_brewer(palette="RdYlBu") +  
                geom_errorbar(aes(x = sample, 
                                  ymin= yaxis_plotted - error_bar_plotted, 
                                  ymax= yaxis_plotted + error_bar_plotted), width=0.25, colour="black", size=0.15)
        return(g)
}