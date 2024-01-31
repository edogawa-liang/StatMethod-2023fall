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


#---------- Scatter of Grade ----------
library(ggplot2)
grade_scatter <- function(data, plot_title){
  title <- paste('Scatter of', plot_title)
  output_plot <- ggplot(data = data) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(aes(x = grade_1, y = grade_2, color = final_grade),
               size = 2.5) +
    scale_color_gradient(low = 'darkred', high = 'deepskyblue') + 
    xlim(0, 20) +
    ylim(0, 20) + 
    labs(title = title,
         x = 'Grade 1',
         y = 'Grade 2',
         color = 'Final Grade') +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(output_plot)
}

m_grade_scatter <- grade_scatter(math_grade, 'Math Grade')
m_grade_scatter
p_grade_scatter <- grade_scatter(portuguese_grade, 'Portuguese Grade')
p_grade_scatter


#---------- Boxplot of Grade ----------
library('reshape2')
variable_boxplot <- function(data, plot_title){
  title <- paste('Boxplot of', plot_title)
  longform <- melt(data = data, id.var = 1)
  output_plot <- ggplot(longform) +
    geom_boxplot(aes(x = variable, y = value)) + 
    ylim(0, 20) +
    labs(title = title,
         y = 'Grade',
         x = '') + 
    scale_x_discrete(labels = c('grade_1' = 'Grade 1',
                                'grade_2' = 'Grade 2',
                                'final_grade' = 'Final Grade')) + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(output_plot)
}

m_grade_box <- variable_boxplot(math_grade, 'Math Grade')
m_grade_box
p_grade_box <- variable_boxplot(portuguese_grade, 'Portuguese Grade')
p_grade_box


#---------- Remove 0 ----------
math_student <- math_student[-which(math_grade$final_grade == 0), ]
math_grade <- math_grade[-which(math_grade$final_grade == 0), ]
portuguese_student <- portuguese_student[-which(portuguese_grade$final_grade == 0), ]
portuguese_grade <- portuguese_grade[-which(portuguese_grade$final_grade == 0), ]

m_grade_scatter <- grade_scatter(math_grade, 'Math Grade')
m_grade_scatter
p_grade_scatter <- grade_scatter(portuguese_grade, 'Portuguese Grade')
p_grade_scatter
m_grade_box <- variable_boxplot(math_grade, 'Math Grade')
m_grade_box
p_grade_box <- variable_boxplot(portuguese_grade, 'Portuguese Grade')
p_grade_box


#---------- Grade Improvement ----------
math_grade <- math_grade %>%
  mutate(grade_1_sd = scale(math_grade$grade_1)[, 1],
         grade_2_sd = scale(math_grade$grade_2)[, 1],
         improvement_sd = grade_2_sd - grade_1_sd,
         grade_1_ct = grade_1 - mean(math_grade$grade_1),
         grade_2_ct = grade_2 - mean(math_grade$grade_2),
         improvement_ct = grade_2_ct - grade_1_ct)

portuguese_grade <- portuguese_grade %>%
  mutate(grade_1_sd = scale(portuguese_grade$grade_1)[, 1],
         grade_2_sd = scale(portuguese_grade$grade_2)[, 1],
         improvement_sd = grade_2_sd - grade_1_sd,
         grade_1_ct = grade_1 - mean(portuguese_grade$grade_1),
         grade_2_ct = grade_2 - mean(portuguese_grade$grade_2),
         improvement_ct = grade_2_ct - grade_1_ct)


#---------- Histo of Grade Improvement ----------
ggplot(melt(math_grade[, 1:3], id.vars = 1)) + 
  geom_histogram(aes(x = value), bins = 20) + 
  facet_grid(variable ~ .)
ggplot(melt(portuguese_grade[, 1:3], id.vars = 1)) + 
  geom_histogram(aes(x = value), bins = 20) + 
  facet_grid(variable ~ .)

ggplot(math_grade) + 
  geom_histogram(aes(x = improvement_ct))
ggplot(portuguese_grade) + 
  geom_histogram(aes(x = improvement_ct))


#---------- Paired t-text ----------
m_pt <- t.test(math_grade$grade_1, math_grade$grade_2, 
               'two.sided', paired = TRUE, conf.level = 0.95)
p_pt <- t.test(portuguese_grade$grade_1, portuguese_grade$grade_2, 
               'two.sided', paired = TRUE, conf.level = 0.95)


#---------- Linear Regression ----------
try1 <- cbind(math_student[, -1], improvement_ct = math_grade$improvement_ct)
model1 <- lm(improvement_ct~., data = try1)
summary(model1)
anova(model1)

try2 <- cbind(math_student[, c(4, 8, 10, 23, 26, 31)], improvement_ct = math_grade$improvement_ct)
model2 <- lm(improvement_ct ~ ., data = try2)
summary(model2)

try3 <- cbind(portuguese_student[, -1], improvement_ct = portuguese_grade$improvement_ct)
model3 <- lm(improvement_ct~., data = try3)
summary(model3)

try4 <- cbind(portuguese_student[, c(2, 4, 10, 14, 25, 30)], improvement_ct = portuguese_grade$improvement_ct)
model4 <- lm(improvement_ct ~ ., data = try4)
summary(model4)


#---------- Each Barplot ----------
library(patchwork)

tryplot <- ggplot(math_student) +
  geom_bar(aes(x = school)) +
  theme_bw()

var_dist <- function(data, plot_title){
  n <- length(colnames(data))
  title <- paste('Distribution of Each Variables (', plot_title, ')')
  output_plot <- ggplot(data) +
    geom_bar(aes(x = school)) +
    theme_bw()
  data_type <- sapply(data, class)
  
  for (i in 3:(n-1)){
    var <- sym(colnames(data)[i])
    temp <- ggplot(data) +
      geom_bar(aes(x = !!var)) +
      theme_bw()
    output_plot <- output_plot + temp 
  }
  
  temp <- ggplot(data) +
    geom_histogram(aes(x = absences)) +
    theme_bw()
  output_plot <- output_plot + temp +
    plot_annotation(title, 
                    theme = theme(plot.title = element_text(hjust = 0.5)))
  
  return(output_plot)
}
m_var_dist <- var_dist(math_student, 'Math')
p_var_dist <- var_dist(portuguese_student, 'Portuguese')


#---------- Heatmap of Correlation ----------
library(corrr)
corr_plot <- function(data, plot_title){
  # correlation
  corr <- data %>%
    select(school:absences) %>%
    correlate()
  for (i in 1:9) {
    for (j in 2:10) {
      if (i < j){
        corr[i, j] <- NA
      }
    }
  }
  corr <- melt(corr, id.vars = 1, na.rm = T)
  cont_level <- c('age', 'class_failures', 'family_relationship', 'free_time', 'social', 'weekday_alcohol', 'weekend_alcohol', 'health', 'absences')
  corr$term <- factor(corr$term,
                      levels = rev(cont_level))
  corr$variable <- factor(corr$variable,
                          levels = cont_level)
  
  # heatmap
  title <- paste('Heatmap of Correlation (', plot_title, ')')
  output_plot <- ggplot(corr, aes(x = variable, y = term, fill = value)) + 
    geom_tile() +
    labs(fill = 'Pearson Correation',
         title = title,
         x = '',
         y = '') +
    geom_text(aes(label = round(value, 2)), color = "white") +
    theme_bw() + 
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.justification = c(0, 1),
          legend.position = c(0.6, 0.95),
          legend.direction = "horizontal") +
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
  return(output_plot)
}
m_s_corr <- corr_plot(math_student, 'Math')
p_s_corr <- corr_plot(portuguese_student, 'Portuguese')

















