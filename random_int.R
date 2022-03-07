library(ragg)
library(showtext)
library(tidyverse)
library(lme4)

# Taken from Mixed Models in R, by M. Clark:  https://m-clark.github.io/mixed-models-with-R/
gpa <- read_csv("https://raw.githubusercontent.com/m-clark/mixed-models-with-R/master/data/gpa.csv")

gpa_sub <- subset(gpa, student <21)
gpa_mixed <- lmer(gpa ~ occasion + (1 | student), data = gpa_sub)
gpa_fixed <- lm(gpa ~ occasion, data = gpa_sub)

coef(gpa_fixed)[[2]]

gpa_sub$rints <- rep(ranef(gpa_mixed)[[1]][[1]],each=6)
gpa_sub$plot_rints <- coef(gpa_fixed)[[1]] + gpa_sub$rints
gpa_sub$slp <- coef(gpa_fixed)[[2]]



theme_set(
  theme_minimal()+
    theme(text = element_text(family="Open Sans"),
          plot.subtitle = element_text(face="italic")
         
  )
)

a <- ggplot(gpa_sub, aes(y=gpa, x=occasion))+
  geom_point(size=2.5)+
  labs(title = "Random-intercept example"
       ,subtite = "subset of 'GPA' dataset")


b <- ggplot(gpa_sub, aes(y=gpa, x=occasion))+
  geom_point(size=2.5)+
  geom_smooth(method = "lm", linetype="dashed", size=1.2)+
  labs(title = "Random-intercept example"
       ,subtite = "subset of 'GPA' dataset")


c <- ggplot(gpa_sub, aes(y=gpa, x=occasion))+
  geom_point(aes(col=as.factor(student)), show.legend = FALSE, size=2.5)+
  geom_smooth(method = "lm", linetype="dashed", size=1.2)+
  labs(title = "Random-intercept example"
       ,subtite = "subset of 'GPA' dataset")

d <- ggplot(gpa_sub, aes(y=gpa, x=occasion))+
  geom_point(aes(col=as.factor(student)), show.legend = FALSE, size=2.5)+
  geom_abline(aes(intercept = plot_rints, slope=slp, col=as.factor(student))
              , alpha=0.5, size=1.2, show.legend = FALSE)+
  # stat_smooth(aes(col=as.factor(student)), method="lm", geom='line', alpha=0.5
  #             , se=FALSE, size=1.2, show.legend = FALSE)+
  geom_smooth(method = "lm", linetype = "dashed", size=1.2)+
  labs(title = "Random-intercept example"
       ,subtite = "subset of 'GPA' dataset")

d

library(animation)

saveGIF(lapply(list(a,b,c,d), print), interval = 4, movie.name="ints.gif"
        , ani.height = 400, ani.width = 600)
