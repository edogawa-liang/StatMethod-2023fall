#---------- Import data ----------
library(tidyverse)
math <- read_csv('student_math_clean.csv')
portuguese <- read_csv('student_portuguese_clean.csv')

math_student <- math %>% select(student_id:absences)
math_grade <- math %>% select(student_id, grade_1:final_grade)

portuguese_student <- portuguese %>% select(student_id:absences)
portuguese_grade <- portuguese %>% select(student_id, grade_1:final_grade)

rm(math)
rm(portuguese)


#---------- As Factor ----------
change2factor <- function(data){
  data <- data %>%
    mutate_if(sapply(data, is.character), as.factor)
  # family size
  levels(data$family_size) <- c('＞3', '≦3')
  data$family_size <- ordered(data$family_size,
                              levels = c('≦3', '＞3'))
  
  # education
  education_levels <- c('5th-9th grade', 'higher', 'none', 'primary(4th grade)', 'secondary')
  levels(data$mother_education) <- education_levels
  levels(data$father_education) <- education_levels
  education_levels <- education_levels[c(3, 4, 1, 5, 2)]
  data$mother_education <- ordered(data$mother_education,
                                   levels = education_levels)
  data$father_education <- ordered(data$father_education,
                                   levels = education_levels)
  
  # travel time
  levels(data$travel_time) <- c('<15', '>60', '15-30', '30-60')
  data$travel_time <- ordered(data$travel_time, 
                              levels = c('<15', '15-30', '30-60', '>60'))
  
  # study time
  levels(data$study_time) <- c('<2', '>10', '2-5', '5-10')
  data$study_time <- ordered(data$study_time, 
                             levels = c('<2', '2-5', '5-10', '>10'))
  
  # ordered 1:5
  data$family_relationship <- ordered(data$family_relationship, 
                                      levels = 1:5)
  data$free_time <- ordered(data$free_time, 
                            levels = 1:5)
  data$social <- ordered(data$social, 
                         levels = 1:5)
  data$weekday_alcohol <- ordered(data$weekday_alcohol, 
                                  levels = 1:5)
  data$weekend_alcohol <- ordered(data$weekend_alcohol, 
                                  levels = 1:5)
  data$health <- ordered(data$health, 
                         levels = 1:5)
  return(data)
}

math_student <- change2factor(math_student)
portuguese_student <- change2factor(portuguese_student)


#---------- Remove 0 ----------
math_student <- math_student[-which(math_grade$final_grade == 0), ]
math_grade <- math_grade[-which(math_grade$final_grade == 0), ]
portuguese_student <- portuguese_student[-which(portuguese_grade$final_grade == 0), ]
portuguese_grade <- portuguese_grade[-which(portuguese_grade$final_grade == 0), ]


#---------- Grade Improvement ----------
math_grade <- math_grade %>%
  mutate(grade_1_ct = grade_1 - mean(math_grade$grade_1),
         grade_2_ct = grade_2 - mean(math_grade$grade_2),
         improvement_ct = grade_2_ct - grade_1_ct,
         grade_1_rank = rank(-grade_1, 'first'),
         grade_2_rank = rank(-grade_2, 'first'),
         improvement_rank = grade_2_rank - grade_1_rank) # 負數:進步

portuguese_grade <- portuguese_grade %>%
  mutate(grade_1_ct = grade_1 - mean(portuguese_grade$grade_1),
         grade_2_ct = grade_2 - mean(portuguese_grade$grade_2),
         improvement_ct = grade_2_ct - grade_1_ct,
         grade_1_rank = rank(-grade_1, 'first'),
         grade_2_rank = rank(-grade_2, 'first'),
         improvement_rank = grade_2_rank - grade_1_rank)


saveRDS(math_student, file = "math_student.Rds")
saveRDS(portuguese_student, file = "portuguese_student.Rds")
saveRDS(math_grade, file = "math_grade.Rds")
saveRDS(portuguese_grade, file = "portuguese_grade.Rds")




















