#---------- Import data ----------
math_student <- readRDS('math_student.Rds')
math_grade <- readRDS('math_grade.Rds')
portuguese_student <- readRDS('portuguese_student.Rds')
portuguese_grade <- readRDS('portuguese_grade.Rds')


#---------- Function ----------
tt_data <- function(X, y, seed, train_ratio){
  data <- cbind(X[, -1], y)
  set.seed(seed)
  train_id <- sample(1:nrow(data), nrow(data)*train_ratio)
  train_data <- data[train_id, ]
  test_data <- data[-train_id, ]
  
  output <- list(train_data, test_data)
  return(output)
}

full_model <- function(data, y_name){
  md <- lm(y ~ ., data = data)
  description1 <- paste('VVVVV Full Model of y :', y_name, 'VVVVV')
  description2<- paste('^^^^^ Full Model of y :', y_name, '^^^^^')
  print(description1)
  print(summary(md))
  print(anova(md))
  print(description2)
}

#---------- Math ----------
m_g1_tt <- tt_data(math_student, math_grade$grade_1, 2, 0.8)
m_g2_tt <- tt_data(math_student, math_grade$grade_2, 2, 0.8)
m_imp_ct_tt <- tt_data(math_student, math_grade$improvement_ct, 2, 0.8)
m_imp_rank_tt <- tt_data(math_student, math_grade$improvement_rank, 2, 0.8)

full_model(m_g1_tt[[1]], 'Grade 1')
full_model(m_g2_tt[[1]], 'Grade 2')
full_model(m_imp_rank_tt[[1]], 'Improvement Rank')
full_model(m_imp_ct_tt[[1]], 'Improvement Center')

md2 <- lm(y ~ school + age + school_choice_reason + class_failures +
            free_time + absences, 
          data = m_imp_ct_tt[[1]])
summary(md2)
anova(md2)

md4 <- lm(y ~ school + age + class_failures +
          free_time + absences, 
          data = m_imp_rank_tt[[1]])
summary(md4)
anova(md4)


#---------- cook
cook2 <- cooks.distance(md2)
max(cook2)
mean(cook2)
cook4 <- cooks.distance(md4)
max(cook4)
mean(cook4)

#---------- dw
library(car)
durbinWatsonTest(md2)
durbinWatsonTest(md4)

#---------- qq
qq1 <- ggplot(data.frame(md2$residuals), aes(sample = md2.residuals)) +
  stat_qq(size = 0.8) +
  stat_qq_line() + 
  theme_bw() + 
  labs(title = 'Q-Q Plot of Residuals(Center Model 2)') + 
  theme(plot.title = element_text(hjust = 0.5))
  
qq2 <- ggplot(data.frame(md4$residuals), aes(sample = md4.residuals)) +
  stat_qq(size = 0.8) +
  stat_qq_line() + 
  theme_bw() + 
  labs(title = 'Q-Q Plot of Residuals(Rank Model 4)') +
  theme(plot.title = element_text(hjust = 0.5))
  
qq1 + qq2

#--------- shapiro.test
shapiro.test(md3$residuals)
shapiro.test(md4$residuals)

#---------- residual
residual2 <- ggplot(md2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = 'Residual vs. Fit. Values Plot(Center Model 2)', 
       x = 'Fitted Values', y = 'Residuals') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

residual4 <- ggplot(md4, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = 'Residual vs. Fit. Values Plot(Rank Model 4)', 
       x = 'Fitted Values', y = 'Residuals') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

residual2 + residual4




#---------- Portuguese ----------
p_g1_tt <- tt_data(portuguese_student, portuguese_grade$grade_1, 1, 0.8)
p_g2_tt <- tt_data(portuguese_student, portuguese_grade$grade_2, 1, 0.8)
p_imp_ct_tt <- tt_data(portuguese_student, portuguese_grade$improvement_ct, 1, 0.8)
p_imp_rank_tt <- tt_data(portuguese_student, portuguese_grade$improvement_rank, 1, 0.8)

full_model(p_g1_tt[[1]], 'Grade 1')
full_model(p_g2_tt[[1]], 'Grade 2')
full_model(p_imp_rank_tt[[1]], 'Improvement Rank')
full_model(p_imp_ct_tt[[1]], 'Improvement Center')

md3 <- lm(grade_1 ~ school + sex + age + mother_education + 
            school_choice_reason + guardian + study_time + 
            class_failures + school_support + higher_ed + 
            social + absences, 
          data = p_g1_tt[[1]])
summary(md3)
anova(md3)

md4 <- lm(grade_2 ~ school + sex + mother_education + 
            school_choice_reason + guardian + study_time + 
            class_failures + school_support + higher_ed + 
            family_relationship + social + absences, 
          data = p_g2_tt[[1]])
summary(md4)
anova(md4)

md6 <- lm(improvement_rank ~ age, 
          data = p_imp_rank_tt[[1]])
summary(md6)
anova(md6)

md8 <- lm(improvement_ct ~ school + age + school + family_relationship, 
          data = p_imp_ct_tt[[1]])
summary(md8)
anova(md8)


#---------- Try 1 ----------
set.seed(1)
test <- sample(as.numeric(rownames(portuguese_grade)), nrow(portuguese_grade)*0.2)
p_test <- cbind(portuguese_student[test, ],
                grade_1 = portuguese_grade[test, 2],
                grade_2 = portuguese_grade[test, 3],
                improvement_rank = portuguese_grade[test, 10])
p_train <- cbind(portuguese_student[-test, ],
                 grade_1 = portuguese_grade[-test, 2],
                 grade_2 = portuguese_grade[-test, 3],
                 improvement_rank = portuguese_grade[-test, 10])

md3 <- lm(grade_1 ~ school + sex + age + mother_education + 
            school_choice_reason + guardian + study_time + 
            class_failures + school_support + higher_ed + 
            social + absences, 
          data = p_train)
summary(md3)
anova(md3)




res <- resid(md3)
plot(fitted(md3), res)
abline(0,0)
qqnorm(res)
qqline(res)
plot(density(res))
plot(p_test$grade_1, predict.lm(md3, p_test),
     xlim = c(0, 15), ylim = c(0, 15))
abline(a = 0, b = 1)



