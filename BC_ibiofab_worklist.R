#Date: 2020.06.10
#Author: Michael Volk 

BC_ibiofab_worklist <- function(BioscreenC_ex = NULL){
        # arguments
        # BioscreenC_ex = "ex15"
        # volume = 200
        
        # possible arguments
        # SourcePlate = "Source"
        # SouceWell = "A1"
        # DestPlate = "Dest1"
        # DestWell = 1
        library(dplyr)
        library(xlsx)
        
        dir <- paste0("BioscreenC/experiments/", BioscreenC_ex, "/position_list/")
        
        # SourceWell and DestWell can be numeric or alpha numeric
        
        worklist <- data.frame()
        well <- 0
        for (i in list.files(dir)){
                data <- read.csv(paste0(dir,i))
                well <- well + 1
                data <- data %>% mutate(SourceWell = well) %>% 
                        mutate(WellNumber = (data$col-1) * 10 + row) %>%
                        mutate(DestPlate = case_when((WellNumber <= 100) ~ "Dest1", (WellNumber > 100) ~ "Dest2")) %>% 
                        mutate(DestWell = case_when((DestPlate == "Dest1") ~ WellNumber, DestPlate == "Dest2" ~ (WellNumber - 100))) %>% 
                        mutate(Volume = volume) %>% mutate(SourcePlate = 1) %>% 
                        select(SourcePlate, SourceWell, DestPlate, DestWell, Volume)
                worklist <- rbind(worklist,data)
        }
        
        write.csv(worklist, file = paste0("BioscreenC/experiments/", BioscreenC_ex, "/worklist.csv"))
        xlsx::write.xlsx(worklist, file = paste0("BioscreenC/experiments/", BioscreenC_ex, "/worklist.xlsx"), row.names = FALSE)
}