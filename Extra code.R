############ Not working significant values filter function - doesn't save to global env

sig_values_funct <- function(df){
  sig_values <- list(NULL)
  for(i in seq_along(df)){
    sig_values[[i]] <- summary(df[[i]]$contrasts) %>% 
      as_tibble(.) %>% 
      filter(`p.value` < .05)
  }
  name <- deparse(substitute(df))
  names(sig_values) <- paste0(names(df))
  assign(
    x = paste0(name, "_rxg"),
    value = sig_values,
    envir = .GlobalEnv)
}

# NO BELOW THIS LINE, OLD STATS


# Descriptives

## Tables

```{r Participant Demographics Table}
nlsy %>% 
  group_by(race, sex) %>% 
  count() %>% 
  knitr::kable()

add %>% 
  group_by(race, sex) %>% 
  count() %>% 
  knitr::kable()
```

```{r Participant Demographics Figure}
nlsy %>% 
  ggplot() +
  geom_bar(aes(race, fill = sex),
           position = "dodge") +
  theme(legend.title = element_blank()) +
  labs(
    x = "Race",
    y = "N"
  )

add %>% 
  ggplot() +
  geom_bar(aes(race, fill = sex),
           position = "dodge") +
  theme(legend.title = element_blank()) +
  labs(
    x = "Race",
    y = "N"
  )
```

```{r}
##### NLSY
proportion <- nlsy %>% 
  group_by(sex, ever_suspended) %>% 
  count() %>% 
  na.omit()
proportion <- spread(proportion, key = ever_suspended, value = n) %>% 
  tibble(.)
proportion$total <- proportion$No + proportion$Yes
proportion$prop <- round(proportion$Yes/proportion$total, 2)
colnames(proportion) <- c("Sex", "Not Suspended", "Suspended", "N", "Proportion")
NLSY_proportion <- proportion
knitr::kable(NLSY_proportion)

####### ADD HEALTH

proportion <- add %>% 
  group_by(sex, ever_suspended) %>% 
  count() %>% 
  na.omit()
proportion <- spread(proportion, key = ever_suspended, value = n) %>% 
  tibble(.)
proportion$total <- proportion$no + proportion$yes
proportion$prop <- round(proportion$yes/proportion$total, 2)
colnames(proportion) <- c("Sex", "Not Suspended", "Suspended", "N", "Proportion")
ADD_proportion <- proportion
knitr::kable(ADD_proportion)

NLSY_proportion %>% 
  ggplot() +
  geom_col(aes(Sex, (Proportion * 100)),
           position = "dodge") +
  labs(title = "Proportion of Students Suspended by Gender",
       subtitle = "NLSY",
       caption = "If bigger bar, disproportiate % of that group are suspended",
       x = "Race",
       y = "Percent of students") +
  theme(legend.position = "none") 

ADD_proportion %>% 
  ggplot() +
  geom_col(aes(Sex, (Proportion * 100)),
           position = "dodge") +
  labs(title = "Proportion of Students Suspended by Race and Gender",
       subtitle = "ADD Health",
       caption = "If bigger bar, disproportiate % of that group are suspended",
       x = "Race",
       y = "Percent of students") +
  theme(legend.position = "none") 
```

```{r Proportion of Suspensions X Race X Gender Table}
##### NLSY
proportion <- nlsy %>% 
  group_by(race, sex, ever_suspended) %>% 
  count() %>% 
  na.omit()
proportion <- spread(proportion, key = ever_suspended, value = n) %>% 
  tibble(.)
proportion$total <- proportion$No + proportion$Yes
proportion$prop <- round(proportion$Yes/proportion$total, 2)
colnames(proportion) <- c("Race", "Sex", "Not Suspended", "Suspended", "N", "Proportion")
NLSY_proportion <- proportion
knitr::kable(NLSY_proportion)

####### ADD HEALTH

proportion <- add %>% 
  group_by(race, sex, ever_suspended) %>% 
  count() %>% 
  na.omit()
proportion <- spread(proportion, key = ever_suspended, value = n) %>% 
  tibble(.)
proportion$total <- proportion$no + proportion$yes
proportion$prop <- round(proportion$yes/proportion$total, 2)
colnames(proportion) <- c("Race", "Sex", "Not Suspended", "Suspended", "N", "Proportion")
ADD_proportion <- proportion
knitr::kable(ADD_proportion)
```

```{r Proportion of Suspensions X Race X Gender Figure}
NLSY_proportion %>% 
  ggplot() +
  geom_col(aes(Race, (Proportion * 100), fill = Sex),
           position = "dodge") +
  labs(title = "Proportion of Students Suspended by Race and Gender",
       subtitle = "NLSY",
       caption = "If bigger bar, disproportiate % of that group are suspended",
       x = "Race",
       y = "Percent of students") +
  theme(legend.position = "none") 

ADD_proportion %>% 
  ggplot() +
  geom_col(aes(Race, (Proportion * 100), fill = Sex),
           position = "dodge") +
  labs(title = "Proportion of Students Suspended by Race and Gender",
       subtitle = "ADD Health",
       caption = "If bigger bar, disproportiate % of that group are suspended",
       x = "Race",
       y = "Percent of students") +
  theme(legend.position = "none") 
```

# Inferential Statistics

## Race X Gender Effects

```{r NLSY INXN BT Race and Sex, results='asis'}
nlsy_race_sex_model <- glm(ever_suspended ~ race * sex,
                           family = binomial, data = nlsy)
summary(nlsy_race_sex_model)

plot_model(nlsy_race_sex_model, type = "pred", terms = c("race", "sex"))

nlsy_suspend_fig <- emmeans(nlsy_race_sex_model, specs = pairwise ~ race | sex,
                            type = "response", adjust = "none")

nlsy_suspend_fig
```

```{r ADD INXN BT Race and Sex, results='asis'}
add_race_sex_model <- glm(ever_suspended ~ race * sex,
                          family = binomial, data = add)
summary(add_race_sex_model)

plot_model(add_race_sex_model, type = "pred", terms = c("race", "sex"))

add_suspend_fig <- emmeans(add_race_sex_model, specs = pairwise ~ race | sex,
                           type = "response", adjust = "none")
add_suspend_fig
```

> This is a reproduction of prior analytic work that shows yes, there are *large racial differences in suspension rate*. 
## Extraversion

## Extraversion 

```{r NLSY Extraversion TIPI}
extrav <- glm(ever_suspended ~ race * sex * extra_tipi,
              family = binomial, data = nlsy)
summary(extrav)

plot_model(extrav, type = "pred", terms = c("extra_tipi", "race", "sex"))

object1 <- emtrends(extrav, pairwise ~ race*sex, var = "extra_tipi", adjust = "none")
object1
```

```{r NLSY Extraversion Mini-IPIP}
extrav <- glm(ever_suspended ~ race * sex * extrav_mi,
              family = binomial, data = nlsy)
summary(extrav)

plot_model(extrav, type = "pred", terms = c("extrav_mi", "race", "sex"))

object1 <- emtrends(extrav, pairwise ~ race*sex, var = "extrav_mi", adjust = "none")
object1
```

```{r ADD Extraversion Mini-IPIP}
extrav <- glm(ever_suspended ~ race * sex * extrav,
              family = binomial, data = add)
summary(extrav)

plot_model(extrav, type = "pred", terms = c("extrav", "race", "sex"))

object1 <- emtrends(extrav, pairwise ~ race*sex, var = "extrav", adjust = "none")
object1
```

> Extraversion seems to be a risk factor for Black students in the ADD Health sample, but not in the NLSY (either through the TIPI or the Mini-IPIP)


## Agreeableness 

```{r NLSY Agreeableness tipi}
agree <- glm(ever_suspended ~ race * sex * agree_tipi,
             family = binomial, data = nlsy)
summary(agree)

plot_model(agree, type = "pred", terms = c("agree_tipi", "race", "sex"))

object1 <- emtrends(agree, pairwise ~ race*sex, var = "agree_tipi", adjust = "none")
object1
```

```{r NLSY Agreeableness mi}
agree <- glm(ever_suspended ~ race * sex * agree_mi,
             family = binomial, data = nlsy)
summary(agree)

plot_model(agree, type = "pred", terms = c("agree_mi", "race", "sex"))

object1 <- emtrends(agree, pairwise ~ race*sex, var = "agree_mi", adjust = "none")
object1
```

```{r ADD Agreeableness mi}
agree <- glm(ever_suspended ~ race * sex * agree,
             family = binomial, data = add)
summary(agree)

plot_model(agree, type = "pred", terms = c("agree", "race", "sex"))

object1 <- emtrends(agree, pairwise ~ race*sex, var = "agree", adjust = "none")
object1
```

> Agreeableness is as predicted

## Conscientiousness 

```{r NLSY Conscientiousness tipi}
consc <- glm(ever_suspended ~ race * sex * consc_tipi,
             family = binomial, data = nlsy)
summary(consc)

plot_model(consc, type = "pred", terms = c("consc_tipi", "race", "sex"))

object1 <- emtrends(consc, pairwise ~ race*sex, var = "consc_tipi", adjust = "none")
object1
```

```{r NLSY Conscientiousness mi}
consc <- glm(ever_suspended ~ race * sex * consc_mi,
             family = binomial, data = nlsy)
summary(consc)

plot_model(consc, type = "pred", terms = c("consc_mi", "race", "sex"))

object1 <- emtrends(consc, pairwise ~ race*sex, var = "consc_mi", adjust = "none")
object1
```

```{r ADD Conscientiousness mi}
consc <- glm(ever_suspended ~ race * sex * consc,
             family = binomial, data = add)
summary(consc)

plot_model(consc, type = "pred", terms = c("consc", "race", "sex"))

object1 <- emtrends(consc, pairwise ~ race*sex, var = "consc", adjust = "none")
object1
```


## Emotional Stability (Neuro) 

```{r NLSY Emotional Stability tipi}
neuro <- glm(ever_suspended ~ race * sex * neuro_tipi,
             family = binomial, data = nlsy)
summary(neuro)

plot_model(neuro, type = "pred", terms = c("neuro_tipi", "race", "sex"))

object1 <- emtrends(neuro, pairwise ~ race*sex, var = "neuro_tipi", adjust = "none")
object1
```

```{r NLSY Emotional Stability mi}
neuro <- glm(ever_suspended ~ race * sex * neuro_mi,
             family = binomial, data = nlsy)
summary(neuro)

plot_model(neuro, type = "pred", terms = c("neuro_mi", "race", "sex"))

object1 <- emtrends(neuro, pairwise ~ race*sex, var = "neuro_mi", adjust = "none")
object1
```

```{r ADD Emotional Stability mi}
neuro <- glm(ever_suspended ~ race * sex * neuro,
             family = binomial, data = add)
summary(neuro)

plot_model(neuro, type = "pred", terms = c("neuro", "race", "sex"))

object1 <- emtrends(neuro, pairwise ~ race*sex, var = "neuro", adjust = "none")
object1
```

## Openness

```{r NLSY Openness tipi}
open <- glm(ever_suspended ~ race * sex * open_tipi,
            family = binomial, data = nlsy)
summary(open)

plot_model(open, type = "pred", terms = c("open_tipi", "race", "sex"))

object1 <- emtrends(open, pairwise ~ race*sex, var = "open_tipi", adjust = "none")
object1
```

```{r NLSY Openness mi}
open <- glm(ever_suspended ~ race * sex * open_mi,
            family = binomial, data = nlsy)
summary(open)

plot_model(open, type = "pred", terms = c("open_mi", "race", "sex"))

object1 <- emtrends(open, pairwise ~ race*sex, var = "open_mi", adjust = "none")
object1
```

```{r ADD Openness mi}
open <- glm(ever_suspended ~ race * sex * open,
            family = binomial, data = add)
summary(open)

plot_model(open, type = "pred", terms = c("open", "race", "sex"))

object1 <- emtrends(open, pairwise ~ race*sex, var = "open", adjust = "none")
object1
```

# Sensitivity Analysis: Examining Variance with Loaded Models

## NLSY TIPI Variance Models

```{r NLSY TIPI Variance Models}
# Model 
nlsy_tipi_variance_model <- glm(ever_suspended ~ 
                                  race * sex * extra_tipi + 
                                  race * sex * agree_tipi + 
                                  race * sex * neuro_tipi + 
                                  race * sex * consc_tipi + 
                                  race * sex * open_tipi,
                                family = binomial, data = )

summary(nlsy_tipi_variance_model)

#### EXTRAVERSION
plot_model(nlsy_tipi_variance_model, 
           type = "pred", 
           terms = c("extra_tipi", "race", "sex"))

nlsy_extra_tipi_var_model <- emtrends(nlsy_tipi_variance_model, 
                                      pairwise ~ race*sex, 
                                      var = "extra_tipi", 
                                      adjust = "none")
nlsy_extra_tipi_var_model

#### AGREEABLENESS
plot_model(nlsy_tipi_variance_model, 
           type = "pred", 
           terms = c("agree_tipi", "race", "sex"))

nlsy_agree_tipi_var_model <- emtrends(nlsy_tipi_variance_model, 
                                      pairwise ~ race*sex, 
                                      var = "agree_tipi", 
                                      adjust = "none")
nlsy_agree_tipi_var_model

#### EMOTIONAL STABILITY
plot_model(nlsy_tipi_variance_model, 
           type = "pred", 
           terms = c("neuro_tipi", "race", "sex"))

nlsy_neuro_tipi_var_model <- emtrends(nlsy_tipi_variance_model, 
                                      pairwise ~ race*sex, 
                                      var = "neuro_tipi", 
                                      adjust = "none")
nlsy_neuro_tipi_var_model

#### CONSCIENTIOUSNESS
plot_model(nlsy_tipi_variance_model, 
           type = "pred", 
           terms = c("consc_tipi", "race", "sex"))

nlsy_consc_var_model <- emtrends(nlsy_tipi_variance_model, 
                                 pairwise ~ race*sex, 
                                 var = "consc_tipi", 
                                 adjust = "none")
nlsy_consc_var_model

#### OPENNESS
plot_model(nlsy_tipi_variance_model, 
           type = "pred", 
           terms = c("open_tipi", "race", "sex"))

nlsy_open_tipi_var_model <- emtrends(nlsy_tipi_variance_model, 
                                     pairwise ~ race*sex, 
                                     var = "open_tipi", 
                                     adjust = "none")
nlsy_open_tipi_var_model
```

## NLSY MINI-IPIP Variance Models

```{r NLSY MINI-IPIP Variance Models}
## Model 
nlsy_mini_ipip_variance_model <- glm(ever_suspended ~ 
                                       race * sex * extrav_mi + 
                                       race * sex * agree_mi + 
                                       race * sex * neuro_mi + 
                                       race * sex * consc_mi + 
                                       race * sex * open_mi,
                                     family = binomial, data = nlsy)

summary(nlsy_mini_ipip_variance_model)

#### EXTRAVERSION
plot_model(nlsy_mini_ipip_variance_model, 
           type = "pred", 
           terms = c("extrav_mi", "race", "sex"))

nlsy_extra_mini_ipip_var_model <- emtrends(nlsy_mini_ipip_variance_model, 
                                           pairwise ~ race*sex, 
                                           var = "extrav_mi", 
                                           adjust = "none")
nlsy_extra_mini_ipip_var_model

#### AGREEABLENESS
plot_model(nlsy_mini_ipip_variance_model, 
           type = "pred", 
           terms = c("agree_mi", "race", "sex"))

nlsy_agree_mini_ipip_var_model <- emtrends(nlsy_mini_ipip_variance_model, 
                                           pairwise ~ race*sex, 
                                           var = "agree_mi", 
                                           adjust = "none")
nlsy_agree_mini_ipip_var_model

#### EMOTIONAL STABILITY
plot_model(nlsy_mini_ipip_variance_model, 
           type = "pred", 
           terms = c("neuro_mi", "race", "sex"))

nlsy_neuro_mini_ipip_var_model <- emtrends(nlsy_mini_ipip_variance_model, 
                                           pairwise ~ race*sex, 
                                           var = "neuro_mi", 
                                           adjust = "none")
nlsy_neuro_mini_ipip_var_model

#### CONSCIENTIOUSNESS
plot_model(nlsy_mini_ipip_variance_model, 
           type = "pred", 
           terms = c("consc_mi", "race", "sex"))

nlsy_consc_mini_ipip_var_model <- emtrends(nlsy_mini_ipip_variance_model, 
                                           pairwise ~ race*sex, 
                                           var = "consc_mi", 
                                           adjust = "none")
nlsy_consc_mini_ipip_var_model

#### OPENNESS
plot_model(nlsy_mini_ipip_variance_model, 
           type = "pred", 
           terms = c("open_mi", "race", "sex"))

nlsy_open_mini_ipip_var_model <- emtrends(nlsy_mini_ipip_variance_model, 
                                          pairwise ~ race*sex, 
                                          var = "open_mi", 
                                          adjust = "none")
nlsy_open_mini_ipip_var_model
```


## ADD Health Variance Models

```{r Add Health Variance Models}
# Model 
add_variance_model <- glm(ever_suspended ~ 
                            race * sex * open + 
                            race * sex * extrav + 
                            race * sex * agree + 
                            race * sex * neuro + 
                            race * sex * consc,
                          family = binomial, data = add)

summary(add_variance_model)

#### EXTRAVERSION
plot_model(add_variance_model, 
           type = "pred", 
           terms = c("extrav", "race", "sex"))

add_extra_variance_model <- emtrends(add_variance_model, 
                                     pairwise ~ race*sex, 
                                     var = "extrav", 
                                     adjust = "none")
add_extra_variance_model

#### AGREEABLENESS
plot_model(add_variance_model, 
           type = "pred", 
           terms = c("agree", "race", "sex"))

add_agree_variance_model <- emtrends(add_variance_model, 
                                     pairwise ~ race*sex, 
                                     var = "agree", 
                                     adjust = "none")
add_agree_variance_model

#### EMOTIONAL STABILITY
plot_model(add_variance_model, 
           type = "pred", 
           terms = c("neuro", "race", "sex"))

add_neuro_variance_model <- emtrends(add_variance_model, 
                                     pairwise ~ race*sex, 
                                     var = "neuro", 
                                     adjust = "none")
add_neuro_variance_model

#### CONSCIENTIOUSNESS
plot_model(add_variance_model, 
           type = "pred", 
           terms = c("consc", "race", "sex"))

add_consc_variance_model <- emtrends(add_variance_model, 
                                     pairwise ~ race*sex, 
                                     var = "consc", 
                                     adjust = "none")
add_consc_variance_model

#### OPENNESS
plot_model(add_variance_model, 
           type = "pred", 
           terms = c("open", "race", "sex"))

add_open_variance_model <- emtrends(add_variance_model, 
                                    pairwise ~ race*sex, 
                                    var = "open", 
                                    adjust = "none")
add_open_variance_model
```

# Exploratory (Add Health)

## Anger

```{r Add Anger}
anger <- glm(ever_suspended ~ race * sex * anger,
             family = binomial, data = add)
summary(anger)

plot_model(anger, type = "pred", terms = c("anger", "race", "sex"))

object1 <- emtrends(anger, pairwise ~ race*sex, var = "anger", adjust = "none")
object1
```



## Optimism

```{r Add Optimism}
optimism <- glm(ever_suspended ~ race * sex * optimism,
                family = binomial, data = add)
summary(optimism)

plot_model(optimism, type = "pred", terms = c("optimism", "race", "sex"))

object1 <- emtrends(optimism, pairwise ~ race*sex, var = "optimism", adjust = "none")
object1
```


## Anxiety 

```{r Add Anxiety}
anxiety <- glm(ever_suspended ~ race * sex * anxiety,
               family = binomial, data = add)
summary(anxiety)

plot_model(anxiety, type = "pred", terms = c("anxiety", "race", "sex"))

object1 <- emtrends(anxiety, pairwise ~ race*sex, var = "anxiety", adjust = "none")
object1
```

# Longitudinal Analyses

## Extraversion
```{r NLSY Extraversion Longitudinal}
extrav <- glm(value ~ race * sex * extra_tipi * school_group,
              family = binomial, data = nlsy_l)
summary(extrav)

plot_model(extrav, type = "pred", terms = c("extra_tipi", "race", "sex", "school_group"))

object1 <- emtrends(extrav, pairwise ~ race * sex * school_group, var = "extra_tipi", adjust = "none")
object1
```


## Agreeableness

```{r NLSY Agreeableness Longitudinal}
agree <- glm(ever_suspended ~ race * sex * agree_tipi * school_group,
             family = binomial, data = nlsy_l)
summary(agree)

plot_model(agree, type = "pred", terms = c("agree_tipi", "race", "sex", "school_group"))

object1 <- emtrends(agree, pairwise ~ race * sex * school_group, var = "agree_tipi", adjust = "none")
object1$contrasts
```

## Neuroticism 

```{r NLSY Neuroticism Longitudinal}
neuro <- glm(ever_suspended ~ race * sex * neuro_tipi * school_group,
             family = binomial, data = nlsy_l)
summary(neuro)

plot_model(neuro, type = "pred", terms = c("neuro_tipi", "race", "sex", "school_group"))

object1 <- emtrends(neuro, pairwise ~ race * sex * school_group, var = "neuro_tipi", adjust = "none")
object1$contrasts

```



## Conscientiousness 

```{r NLSY Conscientiousness Longitudinal}
cons <- glm(ever_suspended ~ race * sex * consc_tipi * school_group,
            family = binomial, data = nlsy_l)
summary(cons)

plot_model(cons, type = "pred", terms = c("consc_tipi", "race", "sex", "school_group"))

object1 <- emtrends(cons, pairwise ~ race * sex * school_group, var = "consc_tipi", adjust = "none")
object1
```

> More protective in hispanic elem than hispanic middle school women: -0.29, p = 0.048)

## Openness 

```{r NLSY Openness Longitudinal}
open <- glm(ever_suspended ~ race * sex * open_tipi * school_group,
            family = binomial, data = nlsy_l)
summary(open)

plot_model(open, type = "pred", terms = c("open_tipi", "race", "sex", "school_group"))

object1 <- emtrends(open, pairwise ~ race * sex * school_group, var = "open_tipi", adjust = "none")
object1
```

#
## Sensitivity Analyses
#
#```{r nlsy_sens Cormat}
#nlsy_sens <- nlsy
#
#nlsy_sens_long <- tibble(
#  "pid" = rep(nlsy_sens$pid, 2),
#  "race" = rep(nlsy_sens$race, 2),
#  "sex" = rep(nlsy_sens$sex, 2),
#  "ever_suspended" = rep(nlsy_sens$ever_suspended, 2),
#  "extraversion" = c(nlsy_sens$extra_tipi, nlsy_sens$extrav_mi),
#  "agreeableness" = c(nlsy_sens$agree_tipi, nlsy_sens$agree_mi),
#  "conscientiousness" = c(nlsy_sens$consc_tipi, nlsy_sens$consc_mi),
#  "neuroticism" = c(nlsy_sens$neuro_tipi, nlsy_sens$neuro_mi),
#  "openness" = c(nlsy_sens$open_tipi, nlsy_sens$open_mi),
#  "measure" = c(rep("tipi", nrow(nlsy_sens)), rep("consc_mi", nrow(nlsy_sens)))
#                )
#```
#
### Extraversion
#
#```{r NLSY Extraversion Sensitivity}
#extrav = nlsy_sens %>% 
#  select(ever_suspended, race, sex, extra_tipi, extrav_mi, pid) %>% 
#  mutate(across(starts_with("extra"), pomp)) %>% 
#  pivot_longer(cols = c(extra_tipi, extrav_mi),
#               names_sep = "_", names_to = c("trait", "scale"),
#               values_to = "score") %>% 
#  filter(!is.nan(score)) %>% 
#  with_groups(pid, ~mutate(., n=n())) %>% 
#  filter(n == 2) %>% 
#  glmer(ever_suspended ~   sex*race*score*scale + (1|pid),
#             family = binomial, data = .)
#summary(extrav)
#
#plot_model(extrav, type = "pred", terms = c("score", "scale", "race"))
#
#```
#
### Agreeableness
#
#```{r NLSY Agreeableness Sensitivity}
#agree = nlsy_sens %>% 
#  select(ever_suspended, race, sex, agree_tipi, agree_mi, pid) %>% 
#  mutate(across(starts_with("agree"), pomp)) %>% 
#  pivot_longer(cols = c(agree_tipi, agree_mi),
#               names_sep = "_", names_to = c("trait", "scale"),
#               values_to = "score") %>% 
#  filter(!is.nan(score)) %>% 
#  with_groups(pid, ~mutate(., n=n())) %>% 
#  filter(n == 2) %>% 
#  glmer(ever_suspended ~   sex*race*score*scale + (1|pid),
#             family = binomial, data = .)
#summary(agree)
#
#plot_model(agree, type = "pred", terms = c("score", "scale", "race"))
#```
#
### Neuroticism 
#
#```{r NLSY Neuroticism Sensitivity}
#neuro = nlsy_sens %>% 
#  select(ever_suspended, race, sex, neuro_tipi, neuro_mi, pid) %>% 
#  mutate(across(starts_with("neuro"), pomp)) %>% 
#  pivot_longer(cols = c(neuro_tipi, neuro_mi),
#               names_sep = "_", names_to = c("trait", "scale"),
#               values_to = "score") %>% 
#  filter(!is.nan(score)) %>% 
#  with_groups(pid, ~mutate(., n=n())) %>% 
#  filter(n == 2) %>% 
#  glmer(ever_suspended ~   sex*race*score*scale + (1|pid),
#             family = binomial, data = .)
#
#summary(neuro)
#
#plot_model(neuro, type = "pred", terms = c("score", "scale", "race"))
#```
#
### Conscientiousness 
#
#```{r NLSY Conscientiousness Sensitivity}
#cons <- nlsy_sens %>% 
#  select(ever_suspended, race, sex, consc_tipi, consc_mi, pid) %>% 
#  mutate(across(starts_with("neuro"), pomp)) %>% 
#  pivot_longer(cols = c(consc_tipi, consc_mi),
#               names_sep = "_", names_to = c("trait", "scale"),
#               values_to = "score") %>% 
#  filter(!is.nan(score)) %>% 
#  with_groups(pid, ~mutate(., n=n())) %>% 
#  filter(n == 2) %>% 
#  glmer(ever_suspended ~   sex*race*score*scale + (1|pid),
#             family = binomial, data = .)
#summary(cons)
#
#plot_model(cons, type = "pred", terms = c("consc", "consc_mi", "race", "sex"))
#```
#
### Openness 
#
#```{r NLSY Openness Sensitivity}
#open <- nlsy_sens %>% 
#  select(ever_suspended, race, sex, open_tipi, open_mi, pid) %>% 
#  mutate(across(starts_with("neuro"), pomp)) %>% 
#  pivot_longer(cols = c(open_tipi, open_mi),
#               names_sep = "_", names_to = c("trait", "scale"),
#               values_to = "score") %>% 
#  filter(!is.nan(score)) %>% 
#  with_groups(pid, ~mutate(., n=n())) %>% 
#  filter(n == 2) %>% 
#  glmer(ever_suspended ~   sex*race*score*scale + (1|pid),
#             family = binomial, data = .)
#summary(cons)
#
#plot_model(cons, type = "pred", terms = c("open_tipi", "open_mi", "race", "sex"))
#```
#
#
#
#