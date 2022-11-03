library(tidyverse)
library(readxl)
library(openxlsx)

star_data <- read_excel("STAR_data.xlsx")

star_teachers <- star_data %>% 
   drop_na() %>%
   group_by(teacher_id = gktchid, 
            class_type = gkclasstype) %>% 
   summarize(reading_score = mean(gktreadss), 
             math_score = mean(gktmathss),
             .groups = "drop")

write.xlsx(star_teachers, file = "STAR_teachers.xlsx")


   
