
marbles$qual <- grepl('Q', marbles$race, fixed = TRUE)


#first standardizing race...

#calculating average time by race and saving as a new df
av_race_times <- marbles %>%
  group_by(race) %>%
  dplyr::summarize(mean_race_time = mean(time_s, na.rm=TRUE))

#then repeating measures for merging
av_race_times <- av_race_times[rep(seq_len(nrow(av_race_times)), each = 16), ]

#merging with original set on id

#but first a sort
marbles<- marbles[
  with(marbles, order(race)),
]


#
marbles_new <- cbind(marbles, av_race_times$mean_race_time)

glimpse(marbles_new)

#let's rename that one...

marbles_new <- rename(marbles_new, average_by_race = `av_race_times$mean_race_time`)

marbles_new <- marbles_new[which(marbles_new$qual == TRUE),  ]

#calculating standard like they did in paper
marbles_new <- marbles_new  %>% 
  dplyr::mutate(standard_time= time_s/average_by_race)

glimpse(marbles_new)


#creating a new median variable by marble for plotting in order
med_st <- marbles_new  %>% 
  group_by(marble_name) %>%
  dplyr::summarize(med_st_time = median(standard_time, na.rm = TRUE ))

#then repeating measures for merging
med_st <- med_st[rep(seq_len(nrow(med_st)), each = 4), ]


marbles_new <- marbles_new[
  with(marbles_new, order(marble_name)),
]

glimpse(marbles_new)

marbles_new1 <- cbind(marbles_new, med_st$med_st_time)



glimpse(marbles_new1)

ggplot(marbles_new1, aes(x=reorder(marble_name, med_st$med_st_time), y=standard_time)) + geom_boxplot() +
labs(title= "Boxplots of Standardized Race Times by Marble Name") + xlab("Marble Name") + ylab("Standardized Race Time") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#creating another median variable by team for plotting in order
med_st_team <- marbles_new  %>% 
  group_by(team_name) %>%
  dplyr::summarize(med_st_team = median(standard_time, na.rm = TRUE ))

#then repeating measures for merging
med_st_team <- med_st_team[rep(seq_len(nrow(med_st_team)), each = 8), ]


marbles_new1 <- marbles_new1[
  with(marbles_new1, order(team_name)),
]


marbles_new1 <- cbind(marbles_new1, med_st_team$med_st_team)



glimpse(marbles_new1)



ggplot(marbles_new1, aes(x=reorder(team_name,  med_st_team$med_st_team), y=standard_time)) + geom_boxplot() +
  labs(title= "Boxplots of Standardized Race Times by Team Name") + xlab("Team Name") + ylab("Standardized Race Time") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))












df <- data.frame(a = 1:2, b = letters[1:2]) 
df[rep(seq_len(nrow(df)), each = 16), ]


marble_av_times <- marble_av_times[
  with(marble_av_times, order(Mean)),
]


marble_av_times <- marbles %>%
  group_by(marble_name) %>%
  dplyr::summarize(Mean = mean(time_s, na.rm=TRUE))

marble_av_times <- marble_av_times[
  with(marble_av_times, order(Mean)),
]

marble_av_times


ggplot(marble_av_times, aes(x=marble_name, y= Mean)) + geom_bar(stat= "identity")



team_av_times <- marbles %>%
  group_by(team_name) %>%
  dplyr::summarize(Mean = mean(time_s, na.rm=TRUE))


library(standardize)
marbles$time_scaled_by_race <- scale_by(time_s ~ track_length_m , marbles)



glimpse(marbles)
marble_av_times <- marbles %>%
  group_by(marble_name) %>%
  dplyr::summarize(Mean = mean(time_scaled_by_race, na.rm=TRUE))

marble_av_times <- marble_av_times[
  with(marble_av_times, order(Mean)),
]



table(marbles$points)
Let's explore what variables predict 

Exploring Data: First I want to check on some simple characteristics of the data like how many races were collected, how many teams, how many marbles, and how many marbles per team.

```{r}

# Looks here like we have an equal number of observations per race with 16 total races
table(marbles$race)


#number of teams. 16 teams
table(marbles$team_name)

#And an equal number of teams in each race
table(marbles$race, marbles$team_name)

# number of individual marbles. 31
table(marbles$marble_name)
table(marbles$marble_name, marbles$race)

#tabling pole position
table(marbles$team_name, marbles$pole)


```

First things first, I'd like to know which team places the highest most often. Let's plot it.

```{r}

# but first we need to convert pole to numeric... 

marbles$pole_new <- as.numeric(sub('.', '', marbles$pole))

glimpse(marbles)
marbles_first <- marbles[ which(marbles$pole_new=="1"), ]

# Barplot
ggplot(marbles_first, aes(x=team_name, y=pole_new) + 
  geom_bar(stat = "identity"))

ggplot(marbles_first, aes(x=team_name, y=pole_new)) + 
  geom_bar(stat = "identity")



```






```{r}

#here i make a new dataset sorted by time and race. i will then assign ranks to plot who ranks highest most often
marble_rank <- marbles[order(marbles$race, marbles$time_s),] 

marble_rank$ranks <- rep(1:16, 16)


table(marble_rank$team_name, marble_rank$ranks)
barplot(table(marble_rank$ranks))

marble_rank$ranks <- as.factor(marble_rank$ranks)

ggplot(marble_rank, aes( y=team_name, x=ranks, fill = team_name)) + 
    geom_bar(position="dodge", stat="identity")


ggplot(marble_rank, aes( x=team_name)) + 
    geom_histogram(stat='count')



ggplot(data, aes(fill=team, y=value, x=specie)) + 
    geom_bar(position="dodge", stat="identity")


ggplot(marble_rank, aes(y=ranks, x=team_name)) + 
    geom_point()

```

#Setting up: Random Seed, Data Split, and Cross Validation 


```{r}

marbles_new <- marbles_new %>%
    dplyr::mutate(team_factor = as.factor(team_name),
                  site_factor = as.factor(site),
                  host_factor = as.factor(host))


set.seed(123)

data_split <- initial_split(marbles_new, prop = .7, strata = "standard_time")

train_data <- training(data_split)
test_data  <- testing(data_split)

folds <- vfold_cv(train_data, v = 5, r=5, strata= "standard_time")
folds

```


Creating a Recipe/Workflow

```{r}

hist(marbles_new$standard_time)


# THEN WE CALCULATE THE RMSE FOR THE NULL MODEL (TRAINING DATA)
RMSE_null_train <- sqrt(sum( (train_data$standard_time - mean(train_data$standard_time))^2 )/nrow(train_data))
print(RMSE_null_train)


glimpse(marbles)


#Recipe() has two arguments: a formula and the data
time_s_recipe <- recipe(standard_time ~ team_factor +  site_factor + host_factor, data = train_data) %>%
  step_dummy(all_nominal_predictors()) #adding step_dummy




#Build a model specification using the parsnip package
lm_mod <- linear_reg() %>%
  set_engine("lm") 

#Model workflow pairs a model and recipe together
time_cont_workflow <- 
  workflow() %>%
  add_model(lm_mod) %>%
  add_recipe(time_s_recipe)


# Fitting the model
time_fit <- 
  time_cont_workflow %>% 
  fit(data = train_data)

# Extracting Model/Recipes with Parsnip
time_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()


# Obtaining Predictions
predict(time_fit , train_data)

time_aug <- 
  augment(time_fit , train_data)

time_aug %>%
  select(standard_time)


# Calculating Root RMSE 
rmse_train <- time_aug %>% 
  rmse(truth = standard_time, .pred)


#COMPARING TO NULL MODEL 

#Looks like our model with all predictors does better than the null at reducing
#RMSE

rmse_train

print(RMSE_null_train)





```
#Fitting a Tree
```{r}
# TREE 

# model specification
tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")    # setting it to regression instead of classification

tune_spec


# tuning grid specification
tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)


tree_grid %>% 
  count(tree_depth)


# Tune a workflow() that bundles together a model
# specification and a recipe or model preprocessor.
# Here we use a workflow() with a straightforward formula; 
# if this model required more involved data preprocessing, 
# we could use add_recipe() instead of add_formula().


tree_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_recipe(time_s_recipe) # using predefined recipe

```



```{r}
# tuning using cross-validation and the tune_grid() function
tree_res <- 
  tree_wf %>% 
  tune_grid(resamples = folds, grid = tree_grid)
```

```{r}
tree_res %>% 
  collect_metrics()

# Once you have done the tuning, you can take a look at some diagnostics
#by sending your object returned from the tune_grid() function to autoplot(). 
#For instance if you tuned the tree and saved the result as tree_tune_res,
#you can run tree_tune_res %>% autoplot(). Depending on the model, the plot
#will be different, but in general it shows you what happened during the tuning process.

#plotting metrics
tree_res %>% autoplot()


# Next, you want to get the model that the tuning process has determined 
# is the best. You can get the best-fit model with select_best() 
# and finalize_workflow() and then do one more fit to the training data with 
# this final workflow using the fit() function. Follow the examples in the tutorial.

# selecting best
best_tree <- tree_res %>%
  select_best(tree_res, metric = "rmse")

best_tree



# finalizing model
final_wf <- 
  tree_wf %>% 
  finalize_workflow(best_tree)

final_wf

# one more fit to the training data with 
# this final workflow using the fit() function

final_fit <- 
  final_wf %>%
  last_fit(data_split) 


# RMSE= 1.23, not much different from the null
final_fit %>%
  collect_metrics()

#Collecting Predictions
tree_pred <- final_fit %>%
  collect_predictions() 

```




```{r}
# Make two plots, one that shows model predictions from the tuned model 
# versus actual outcomes
  
ggplot(data=tree_pred, aes(x=.pred, y=standard_time)) + geom_point() + labs(title= "Plot of Model Predictions from Tuned Model vs Actual Outcomes", 
       x= "Model Predictions", y= "Actual Outcomes") 

#calculating residuals
tree_pred$resid <- tree_pred$standard_time - tree_pred$.pred 

# one that plots residuals.
# plotting residuals 
ggplot(data=tree_pred, aes(x=.pred , y=resid)) + geom_point() +
  labs(title= "Plot of Model Predictions from Tuned Model vs Actual Outcomes", 
       x= "Model Predictions", y= "Residuals") 



# Look at/print the model performance and compare it with the null model
# (still only on training data). Here, we want the performance of the tuned, 
# best-fitting model on the CV dataset (we are not yet touching the test data). 
# You can get that for instance with the show_best() function, which gives you
# the mean cross-validated performance for the best models. It also shows the
# standard deviation for the performance. Compare that model performance with the null model

```


###Comparing RMSE to Null

The tree model does not perform very well, and the model only predicts a few discrete
outcome values. That’s also noticeable when we compare RMSE for the tree model(1.23) 
and the null model (1.21). They are very similar.

```{r}
# RMSE= 1.23
show_best(final_fit, metric= "rmse")

# Null Model. RMSE 1.21
rmse_train
```


###Tree Plot
```{r}
final_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

```


### Estimate variable importance based on the model’s structure.
```{r}
library(vip)

final_fit %>% 
  extract_fit_parsnip() %>% 
  vip()
```


