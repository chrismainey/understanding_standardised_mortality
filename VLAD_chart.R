library(COUNT)
library(tidyverse)

# Load the 'medpar' dataset.  See help, or type ?medpar into console for more details.
data("medpar")


# build logistic regression model for risk of death
mod1 <- glm(died ~ los + factor(type) + age80, data=medpar, family = "binomial")
summary(mod1)

# Predict risk of death back into data frame.
medpar$pred <- predict(mod1, type="response")

# Subset for just one organisation: "030001"
# ps = probability of survival (1-'pred' the probability of death)
sub <- 
  medpar %>% 
  filter(provnum == "030001") %>% 
  mutate(ps = 1 - pred,
         lives = ifelse(died == 1, -ps, (1-ps)))

# Cumulative sum over weight
sub$wt <- cumsum(sub$lives)

ggplot(sub, aes(y=wt, x=seq(length(lives))))+
  geom_point()+
  geom_line()+
  theme(text = element_text(size=9))+
  labs(title = "VLAD chart of `medpar` data set from COUNT package"
       , y = "Lives gained"
       , x = "Patients discharged")+ 
  theme(text = element_text(size=9), plot.title = element_text(size=16),
        axis.title = element_text(size=11))