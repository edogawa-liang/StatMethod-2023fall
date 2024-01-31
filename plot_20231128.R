#---------- Import data ----------
library(tidyverse)
math_student <- readRDS('math_student.Rds')
math_grade <- readRDS('math_grade.Rds')
portuguese_grade <- readRDS('portuguese_grade.Rds')
portuguese_student <- readRDS('portuguese_student.Rds')

#---------- Scatter of Grade ----------
library(ggplot2)
library(ggExtra)
grade_scatter <- function(data, plot_title){
  title <- paste('Scatter of', plot_title)
  output_plot <- ggplot(data = data) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(aes(x = grade_1, y = grade_2, color = final_grade)) +
    scale_color_gradient(low = 'darkred', high = 'deepskyblue',
                         limits = c(0, 20)) + 
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
ggsave('plot/math_grade_scatter.png', m_grade_scatter, 
       width = 12, height = 10, units = 'cm')


#---------- Boxplot of Grade ----------
library(reshape2)
variable_boxplot <- function(data, plot_title){
  title <- paste('Boxplot of', plot_title)
  data <- data %>%
    select(student_id:final_grade)
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
ggsave('plot/math_grade_box.png', m_grade_box, 
       width = 8, height = 10, units = 'cm')
p_grade_box <- variable_boxplot(portuguese_grade, 'Portuguese Grade')
ggsave('plot/portuguese_grade_box.png', p_grade_box, 
       width = 8, height = 10, units = 'cm')


#---------- Histo of Grade Improvement ----------
library(patchwork)
grade_histo <- function(data){
  title <- paste('Histiogram of Grades')
  data <- melt(data[, c(1:3, 7, 10)], id.vars = 1)
  data$variable <- factor(data$variable,
                          labels = c('Grade 1', 'Grade 2', 
                                     'Impro(Decent)', 'Impro(Rank)'))
  output_plot <- ggplot(data) + 
    geom_histogram(aes(x = value), bins = 20) + 
    facet_grid(variable ~ .) + 
    labs(x = 'Grade', y = 'Count',
         title = title) + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(output_plot)
}

histo4 <- function(data){
  title <- 'Histogram of Grades'
  
  output_plot <- ggplot(data) +
    geom_histogram(aes(x = grade_1), bins = 20) +
    theme_bw() +
    labs(x = colnames(data)[2], y = 'Count')
  
  for (i in c(3, 7, 10)){
    var <- sym(colnames(data)[i])
    temp <- ggplot(data) +
      geom_histogram(aes(x = !!var)) +
      theme_bw() +
      labs(x = colnames(data)[i], y = 'Count')
    output_plot <- output_plot + temp 
  }
  output_plot +
    plot_annotation(title, 
                    theme = theme(plot.title = element_text(hjust = 0.5)))
}
histo4(math_grade)

m_grade_histo <- grade_histo(math_grade)
ggsave('plot/math_grade_histo.png', m_grade_histo, 
       width = 8.5, height = 14, units = 'cm')


#---------- Each Barplot ----------
library(patchwork)
var_dist <- function(data, plot_title){
  n <- length(colnames(data))
  title <- paste('Distribution of Each Variables (', plot_title, ')')
  output_plot <- ggplot(data) +
    geom_bar(aes(x = school)) +
    theme_bw() +
    theme(axis.text = element_text(size = 6),
          axis.title = element_text(size = 8))  
  data_type <- sapply(data, class)
  
  for (i in 3:(n-1)){
    var <- sym(colnames(data)[i])
    temp <- ggplot(data) +
      geom_bar(aes(x = !!var)) +
      theme_bw() +
      theme(axis.text = element_text(size = 6),
            axis.title = element_text(size = 8))   
    output_plot <- output_plot + temp 
  }
  
  temp <- ggplot(data) +
    geom_histogram(aes(x = absences)) +
    xlim(c(0, 80)) + 
    theme_bw() +
    theme(axis.text = element_text(size = 6),
          axis.title = element_text(size = 8))
  output_plot <- output_plot + temp +
    plot_annotation(title, 
                    theme = theme(plot.title = element_text(hjust = 0.5))) +
    plot_layout(ncol = 5)
  
  return(output_plot)
}
m_var_dist <- var_dist(math_student, 'Math')
ggsave('plot/math_student_variables.png', m_var_dist, 
       width = 20, height = 20, units = 'cm')


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
ggsave('plot/math_student_correlation.png', m_s_corr, 
       width = 20, height = 20, units = 'cm')


#---------- Q-Q Plot ----------
qq_plot <- function(data, plot_title){
  title <- paste('Q-Q Plot of Each Grade (', plot_title, ')')
  
  pl1 <- ggplot(data, aes(sample = grade_1)) +
    stat_qq(size = 0.8) +
    stat_qq_line() + 
    theme_bw() + 
    labs(title = 'Grade 1') + 
    theme(plot.title = element_text(hjust = 0.5))
  pl2 <- ggplot(data, aes(sample = grade_2)) +
    stat_qq(size = 0.8) +
    stat_qq_line() + 
    theme_bw() + 
    labs(title = 'Grade 2') + 
    theme(plot.title = element_text(hjust = 0.5))
  pl3 <- ggplot(data, aes(sample = improvement_ct)) +
    stat_qq(size = 0.8) +
    stat_qq_line() + 
    theme_bw() + 
    labs(title = 'Improvement ( Center )') + 
    theme(plot.title = element_text(hjust = 0.5))
  pl4 <- ggplot(data, aes(sample = improvement_rank)) +
    stat_qq(size = 0.8) +
    stat_qq_line() + 
    theme_bw() + 
    labs(title = 'Improvement ( Rank )') + 
    theme(plot.title = element_text(hjust = 0.5))
  
  output_plot <- pl1 + pl2 + pl3 + pl4 +
    plot_annotation(title, 
                    theme = theme(plot.title = element_text(hjust = 0.5)))
  return(output_plot)
}
m_g_qqplot <- qq_plot(math_grade, 'Math')
ggsave('plot/math_grade_qqplot.png', m_g_qqplot, 
       width = 18, height = 18, units = 'cm')









