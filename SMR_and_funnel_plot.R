library(COUNT)
library(tidyverse)

# Load the 'medpar' dataset.  See help, or type ?medpar into console for more details.
data("medpar")


# build logistic regression model for risk of death
mod1 <- glm(died ~ los + factor(type) + age80, data=medpar, family = "binomial")
summary(mod1)

# Predict risk of death back into data frame.
medpar$pred <- predict(mod1, type="response")



# SMRs
medpar %>% 
  group_by(provnum) %>% 
  summarise(Observered = sum(died),
            Expected = sum(pred),
            SMR = sum(died) / sum(pred))


# Funnel plot
library(FunnelPlotR)

a<-funnel_plot(medpar$died, medpar$pred, medpar$provnum, 
               draw_adjusted = FALSE, draw_unadjusted = TRUE 
               , x_range = c(0,30), y_range=c(0,3)
               , title = "Funnel Plot of SMR, using `medpar` data from COUNT package" )

a

# Details on outliers
outliers(a)

# Extract overdispersion parameters - in this case, no overdispersion so they report 0
phi(a)

tau2(a)
