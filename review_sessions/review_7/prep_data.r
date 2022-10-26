library(tidyverse)
library(readxl)
library(openxlsx)

star_data <- read_excel("STAR_data.xlsx")

star_teachers <- star_data %>% 
   drop_na() %>%
   group_by(gktchid, gkclasstype) %>% 
   summarize(across(c(gktreadss, gktmathss, hsactread, hsactmath), mean),
             .groups = "drop")

write.xlsx(star_teachers, file = "STAR_teachers.xlsx")


   
