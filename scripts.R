library(MASS)
library(magrittr)
library(tidyverse)

id <- "1HZHK4qTTFTBDOqajQk2vLBUixaNE0X2l" # google file ID
hatecrimes <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))



hatecrimes %<>%
  filter(state!="Hawaii") %>%
  mutate(trump_state = case_when(share_voters_voted_trump>=0.5 ~ 1,
                                                         TRUE ~ 0))

sum(hatecrimes$trump_state) # =25 -> equal number of republican and non-republican states


#Model 1: Linear Regression 

linear_model <- hatecrimes %>%
  filter(state!="District of Columbia") %>%
  dplyr::select(-c(state, trump_state, hate_crimes_per_100k_splc)) %>%
  #dplyr::select(-c(share_voters_voted_trump)) %>% #for fbi data
  lm(avg_hatecrimes_per_100k_fbi ~ ., .)

summary(linear_model)
step(linear_model)
#Only significant factor is the gini index 
plot(linear_model,c(4,5))

order(-hatvalues(linear_model))


#Model 2: Linear Regression (Trump States)

hatecrimes %>%
  #filter(state!="District of Columbia") %>%
  filter(trump_state==1) %>%
  dplyr::select(-c(trump_state,state, share_voters_voted_trump, hate_crimes_per_100k_splc)) %>%
  lm(avg_hatecrimes_per_100k_fbi ~ . , .) %>%
  summary()
#No Significant Factors



#Model 3: Linear Regression (Non-Trump States)

hatecrimes %>%
  #filter(state!="District of Columbia") %>%
  filter(trump_state==0) %>%
  dplyr::select(-c(trump_state,state, share_voters_voted_trump, hate_crimes_per_100k_splc)) %>%
  lm(avg_hatecrimes_per_100k_fbi ~ . , .) %>%
  summary()
#Significant factors include: Gini Index, Share_non_citizen, share_population_with_high_school_degree


hatecrimes %>% filter(trump_state==0) %>% .[['hate_crimes_per_100k_splc']] %>% na.omit() %>% mean()

hatecrimes %>% filter(trump_state==0) %>% .[['avg_hatecrimes_per_100k_fbi']]%>% na.omit() %>%  var()

hate_crimes_trump_states <- hatecrimes %>% filter(trump_state==1) %>% .[['hate_crimes_per_100k_splc']]  %>% na.omit()
hate_crimes_non_trump_states <- hatecrimes %>% filter(trump_state==0) %>% .[['hate_crimes_per_100k_splc']] %>% na.omit()

t.test(hate_crimes_trump_states, hate_crimes_non_trump_states, var.equal = F) # can't reject the hypothesis that 

# Do republican states have a different relationship between income levels + hate crime?

  

hatecrimes %>%
  #filter(state!="District of Columbia") %>%
  ggplot(aes(y = avg_hatecrimes_per_100k_fbi, x = share_non_citizen, colour = as.factor(trump_state), group=trump_state)) +
 # scale_color_brewer(palette ="Spectral", name="republican_state") +
  geom_smooth(method = "lm", se= T) +
 # labs(title="", x="Income Inequality", y="Average Hate Crime") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 


# Proportion Models 1: total % of US hate crimes

model_proportion_beta <-hatecrimes %>%
  #filter(state!="District of Columbia") %>%
  mutate(prop_fbi = avg_hatecrimes_per_100k_fbi/sum(avg_hatecrimes_per_100k_fbi))%>%
  dplyr::select(-c(state,hate_crimes_per_100k_splc,avg_hatecrimes_per_100k_fbi, trump_state)) %>%
  drop_na()  %>%
  betareg::betareg( prop_fbi ~ ., .)

summary(model_proportion_beta)

# Proportion Models 1: total % of US hate crimes
model_proportion_binomial <-hatecrimes %>%
  filter(state!="District of Columbia") %>%
  mutate(prop_fbi = avg_hatecrimes_per_100k_fbi/sum(avg_hatecrimes_per_100k_fbi))%>%
  dplyr::select(-c(state,hate_crimes_per_100k_splc,avg_hatecrimes_per_100k_fbi, trump_state)) %>%
  drop_na()  %>%
  glm(prop_fbi ~ ., family=quasibinomial, .)

summary(model_proportion_binomial)


# Proportion Models 2: total % of US hate crimes
model_proportion_beta_2 <-hatecrimes %>%
 # filter(state!="District of Columbia") %>%
  mutate(prop_fbi = avg_hatecrimes_per_100k_fbi/sum(avg_hatecrimes_per_100k_fbi))%>%
  dplyr::select(-c(state,hate_crimes_per_100k_splc,avg_hatecrimes_per_100k_fbi, trump_state)) %>%
  drop_na()  %>%
  betareg::betareg(prop ~ ., .)

summary(model_proportion_beta_2)

#Poisson Model

model_poisson<-hatecrimes  %>%
  filter(state!="District of Columbia") %>%
  dplyr::select(-c(state,hate_crimes_per_100k_splc, trump_state)) %>%
  #dplyr::select(-c(state,avg_hatecrimes_per_100k_fbi, trump_state)) %>%
  drop_na()  %>%
  glm(avg_hatecrimes_per_100k_fbi ~ ., family=quasipoisson(link="identity"), .)

summary(model_poisson)

pearson_deviance <- sum(residuals(model_poisson,type="pearson")**2)

1-pchisq(pearson_deviance, 9)
