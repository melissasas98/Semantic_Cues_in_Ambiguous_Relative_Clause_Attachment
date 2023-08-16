library(lme4)
library(lmerTest)
library(sjPlot)
library(tidyverse)
library(emmeans)
library(see)
library(car)
library(gridExtra)

#read in data
df <- read_csv("Results/evaluated_data.csv")

#preprocess variables for the model
df.2 <- df %>%
  mutate(number = as.factor(number)) %>%
  mutate(model = as.factor(model)) %>%
  mutate(size = as.factor(size)) %>%
  mutate(relative_pronoun = as.factor(relative_pronoun))

#define statistical model
model <- glmer(accuracy ~ relative_pronoun * number + model * size + (1 | id),  
               data = df.2,
               family = binomial,
               control = glmerControl(optimizer="bobyqa", 
               optCtrl=list(maxfun=2e5)))

#evaluate the model
Anova(model)
emmeans(model, ~ relative_pronoun*number*model*size)

#plot model for the large models
plot_large <- plot_model(model, type = "pred", 
                         terms = c("number", "relative_pronoun", "model", "size")) +
  theme_minimal() +
  xlab("Number") + ylab("Accuracy") + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
  labs(color = "Relative Pronoun") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))+
  ggtitle("Predicted Values of Accuracy")

#change order of the values in the size variable
df.2$size <- relevel(df.2$size, ref = "large") 
#reload model
model <- glmer(accuracy ~ relative_pronoun * number + model * size + (1 | id),  
               data = df.2,
               family = binomial,
               control = glmerControl(optimizer="bobyqa", 
               optCtrl=list(maxfun=2e5)))

#plot model for the base models
plot_base <- plot_model(model, type = "pred", 
                        terms = c("number", "relative_pronoun", "model", "size")) +
  theme_minimal() +
  xlab("Number") + ylab("Accuracy") + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))+
  labs(color = "Relative Pronoun") +
  ggtitle("Predicted Values of Accuracy")

#print plots for both model sizes
combined_plot <- grid.arrange(plot_large, plot_base, ncol = 2)

#retrieve the percentage of the auxiliary predictions for the large models
df.3.large<- df.2 %>%
  mutate(accuracy = as.factor(accuracy)) %>%
  group_by(model, size, accuracy, predicted_token) %>%
  filter(predicted_token != "NA") %>% 
  filter(size == "large") %>%
  count(predicted_token) %>% 
  rename(predicted_token_count = n) %>%
  group_by(accuracy, model) %>%
  mutate(prediction_percentage = 
         predicted_token_count / sum(predicted_token_count) * 100)

#plot the percentage of the auxiliary predictions for the large models
ggplot(df.3.large, aes(x = predicted_token, y = prediction_percentage, 
                       fill = interaction(accuracy, model))) + 
  scale_fill_discrete(name = "Rating of the Large Models", 
                      labels = c("Correct-BERT", "Incorrect-BERT", 
                                 "Correct-RoBERTa", "Incorrect-RoBERTa")) +
  labs(x = "Predicted Token", y = "Amount of Predictions (Percentages)") + 
  scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100), 
                     labels = function(x) paste0(x, "%")) +
  ggtitle("Predictions of 'was' and 'were' of the Large Models") +
  theme_minimal() + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))+
  geom_bar(stat = "identity", position = "dodge")

#retrieve the percentage of the auxiliary predictions for the base models
df.3.base<- df.2 %>%
  mutate(accuracy = as.factor(accuracy)) %>%
  group_by(model, size, accuracy, predicted_token) %>%
  filter(predicted_token != "NA") %>% 
  filter(size == "base") %>%
  count(predicted_token) %>% 
  rename(predicted_token_count = n) %>%
  group_by(accuracy, model) %>%
  mutate(prediction_percentage = 
         predicted_token_count / sum(predicted_token_count) * 100)

#plot the percentage of the auxiliary predictions for the large models
ggplot(df.3.base, aes(x = predicted_token, y = prediction_percentage, 
                      fill = interaction(accuracy, model))) + 
  scale_fill_discrete(name = "Rating of the Base Models", 
                      labels = c("Correct-BERT", "Incorrect-BERT", 
                                 "Correct-RoBERTa", "Incorrect-RoBERTa")) +
  labs(x = "Predicted Token", y = "Amount of Predictions (Percentages)") + 
  scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100), 
                     labels = function(x) paste0(x, "%")) +
  ggtitle("Predictions of 'was' and 'were' of the Base Models") +
  theme_minimal() + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))+
  geom_bar(stat = "identity", position = "dodge")

#retrieve percentage of predictions of "was" for the first four position intervals for the large models
df.4.large <- df.2 %>%
  mutate(accuracy = as.factor(accuracy)) %>%
  group_by(model, size, accuracy, was_position_interval) %>%
  filter(was_position_interval != "5") %>% 
  filter(size == "large") %>%
  count(was_position_interval) %>% 
  rename(was_count = n) %>%
  group_by(model, accuracy) %>%
  mutate(was_percentage = was_count / sum(was_count) * 100)

#plot percentage of predictions of "was" for the first four position intervals for the large models
ggplot(df.4.large, aes(x = was_position_interval, y = was_percentage, 
                       fill = interaction(accuracy, model))) + 
  scale_fill_discrete(name = "Rating of the Large Models", 
                      labels = c("Correct-BERT", "Incorrect-BERT", 
                                 "Correct-RoBERTa", "Incorrect-RoBERTa")) +
  scale_x_discrete(name = "Position Interval", 
                   limits = c("1-5", "6-10", "11-15", "16-20")) +
  scale_y_continuous(name = "Amount of 'was' Predictions (Percentages)",
                     breaks = seq(0, 100, by = 20), limits = c(0, 100), 
                     labels = function(x) paste0(x, "%")) +
  theme_minimal() + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))+
  ggtitle("Distribution of 'was' Predictions of the Large Models'") +
  geom_bar(stat = "identity", position = "dodge")

#retrieve percentage of predictions of "was" for the first four position intervals for the base models
df.4.base <- df.2 %>%
  mutate(accuracy = as.factor(accuracy)) %>%
  group_by(model, size, accuracy, was_position_interval) %>%
  filter(was_position_interval != "5") %>% 
  filter(size == "base") %>%
  count(was_position_interval) %>% 
  rename(was_count = n) %>%
  group_by(model, accuracy) %>%
  mutate(was_percentage = was_count / sum(was_count) * 100)

#plot percentage of predictions of "was" for the first four position intervals for the base models
ggplot(df.4.base, aes(x = was_position_interval, y = was_percentage, 
                      fill = interaction(accuracy, model))) + 
  scale_fill_discrete(name = "Rating of the Base Models", 
                      labels = c("Correct-BERT", "Incorrect-BERT", 
                                 "Correct-RoBERTa", "Incorrect-RoBERTa")) +
  scale_x_discrete(name = "Position Interval", 
                   limits = c("1-5", "6-10", "11-15", "16-20")) +
  scale_y_continuous(name = "Amount of 'was' Predictions (Percentages)",
                     breaks = seq(0, 100, by = 20),
                     limits = c(0, 100),
                     labels = function(x) paste0(x, "%")) +
  theme_minimal() + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))+
  ggtitle("Distribution of 'was' Predictions of the Base Models'") +
  geom_bar(stat = "identity", position = "dodge")

#retrieve percentage of predictions of "were" for the first four position intervals for the large models
df.5.large <- df.2 %>%
  mutate(accuracy = as.factor(accuracy)) %>%
  group_by(model, size, accuracy, were_position_interval) %>%
  filter(were_position_interval != "5") %>% 
  filter(size == "large") %>%
  count(were_position_interval) %>% 
  rename(were_count = n) %>%
  group_by(model, accuracy) %>%
  mutate(were_percentage = were_count / sum(were_count) * 100)

#plot percentage of predictions of "were" for the first four position intervals for the large models
ggplot(df.5.large, aes(x = were_position_interval, y = were_percentage, 
                       fill = interaction(accuracy, model))) + 
  scale_fill_discrete(name = "Rating of the Large Models", 
                      labels = c("Correct-BERT", "Incorrect-BERT", 
                                 "Correct-RoBERTa", "Incorrect-RoBERTa")) +
  scale_x_discrete(name = "Position Interval", 
                   limits = c("1-5", "6-10", "11-15", "16-20")) +
  scale_y_continuous(name = "Amount of 'were' Predictions (Percentages)",
                     breaks = seq(0, 100, by = 20),
                     limits = c(0, 100),
                     labels = function(x) paste0(x, "%")) +
  theme_minimal() + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))+
  ggtitle("Distribution of 'were' Predictions of the Large Models") +
  geom_bar(stat = "identity", position = "dodge")

#retrieve percentage of predictions of "were" for the first four position intervals for the base models
df.5.base <- df.2 %>%
  mutate(accuracy = as.factor(accuracy)) %>%
  group_by(model, size, accuracy, were_position_interval) %>%
  filter(were_position_interval != "5") %>% 
  filter(size == "base") %>%
  count(were_position_interval) %>% 
  rename(were_count = n) %>%
  group_by(model, accuracy) %>%
  mutate(were_percentage = were_count / sum(were_count) * 100)

#plot percentage of predictions of "were" for the first four position intervals for the base models
ggplot(df.5.base, aes(x = were_position_interval, y = were_percentage, 
                      fill = interaction(accuracy, model))) + 
  scale_fill_discrete(name = "Rating of the Base Models", 
                      labels = c("Correct-BERT", "Incorrect-BERT", 
                                 "Correct-RoBERTa", "Incorrect-RoBERTa")) +
  scale_x_discrete(name = "Position Interval", 
                   limits = c("1-5", "6-10", "11-15", "16-20")) +
  scale_y_continuous(name = "Amount of 'were' Predictions (Percentages)",
                     breaks = seq(0, 100, by = 20),
                     limits = c(0, 100),
                     labels = function(x) paste0(x, "%")) +
  theme_minimal() + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))+
  ggtitle("Distribution of 'were' Predictions of the Base Models") +
  geom_bar(stat = "identity", position = "dodge")

#calculate difference between the positions of "was" and "were"
df.2$difference <- abs(df.2$was_position - df.2$were_position)

#restrict difference values to be below 151
df.6<-df.2[!(df.2$difference>=150),]

#change text of the labels of the plots
supp.labs <- c('1' = "Correct Predictions", '0' = "Incorrect Predictions")

#retrieve the percentages of the difference values for the large models
df.7.large <- df.6 %>%
  mutate(accuracy = as.factor(accuracy)) %>%
  group_by(model, size, accuracy, difference) %>%
  filter(size == "large") %>%
  count(difference) %>% 
  rename(difference_count = n) %>%
  group_by(model) %>%
  mutate(difference_percentage = difference_count / sum(difference_count) * 100)

#plot the percentages of the difference values for the large models
ggplot(df.7.large, aes(x = difference_percentage, y = difference, 
                       colour = model)) +
  geom_segment(aes(x = 0, xend = difference_percentage, y = difference, 
                   yend = difference)) +
  labs(colour = "Model Type") +
  labs(x = "Amount of the Difference in Positions (Percentages)", 
       y = "Difference in Positions of 'was' and 'were'") + 
  scale_x_continuous(breaks = seq(0, 5),limits = c(0, 5), 
                     labels = function(x) paste0(x, "%")) +
  ggtitle("Difference in Distribution of the Auxiliery Positions of the Large Models") +
  facet_wrap(~accuracy, scales = "free_x", labeller = as_labeller(supp.labs)) +
  theme_minimal() + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))+
  geom_point()

#retrieve the percentages of the difference values for the base models
df.7.base <- df.6 %>%
  mutate(accuracy = as.factor(accuracy)) %>%
  group_by(model, size, accuracy, difference) %>%
  filter(size == "base") %>%
  count(difference) %>% 
  rename(difference_count = n) %>%
  group_by(model) %>%
  mutate(difference_percentage = difference_count / sum(difference_count) * 100)

#plot the percentages of the difference values for the base models
ggplot(df.7.base, aes(x = difference_percentage, y = difference, 
                      colour = model)) +
  geom_segment(aes(x = 0, xend = difference_percentage, y = difference, 
                   yend = difference)) +
  labs(colour = "Model Type") +
  labs(x = "Amount of the Difference in Positions (Percentages)", 
       y = "Difference in Positions of 'was' and 'were'") + 
  scale_x_continuous(breaks = seq(0, 5),limits = c(0, 5), 
                     labels = function(x) paste0(x, "%")) +
  ggtitle("Difference in Distribution of the Auxiliery Positions of the Base Models") +
  facet_wrap(~accuracy, scales = "free_x", labeller = as_labeller(supp.labs)) +
  theme_minimal() + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))+
  geom_point()