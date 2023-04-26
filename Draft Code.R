install.packages('bookdown')
pacman::p_load(tidyverse, tidymodels,ggplot2,
               skimr,forcats,purrr,inspectdf,corrplot,vip)

start_exam <- function(ID) {
  ### check valid ID
  if (!is.numeric(ID)) {
    stop("Please set ID to your student number. For example, if your student number is a1234567, then please set ID to 1234567.")
  }
  if (as.integer(ID) - ID != 0) {
    stop("Please set ID to your student number. For example, if your student number is a1234567, then please set ID to 1234567.")
  }
  ### set seed as year +_ ID
  seed <- as.integer(format(Sys.time(), "%Y")) + ID
  set.seed(seed)
  i <- sample(3,1)
  ### specify methods and responses
  comb <- tibble(method = c("lasso regression",
                            "ridge regression",
                            "random forest"),
                 response = c(rep("price", 3)))
  ### set method and response
  M1 <- comb[i,]
  ### give method
  print(glue::glue("Please fit a {M1$method} model to the data returned by this function, specifying {M1$response} as the response variable."))
  ### random sample of data
  data(diamonds, package = "ggplot2")
  data <- diamonds[,c(7,sample(c(1:6,8:10))[-1])] %>%
    sample_frac(.8)
  return(data)
}

?sample
my_data=start_exam(1846505)
summary(my_data)
?diamonds
View(my_data)
##EDA
?diamonds
my_data%>%
  ggplot(aes(x=price))+
  geom_histogram()+
  ggtitle('price distribution')

 
my_data=my_data%>%
  mutate(color=as.factor(color),
         cut=as.factor(cut))
my_data_tranformed<-my_data%>%
  mutate(price=log(price))
my_data_tranformed%>%
  select_if(is.numeric)%>%
  select(-price)%>%
  cor()%>%
  corrplot(title='correlations between all predictors',
           mar=c(0,0,1,0))
rf_tune[1]
?corrplot
my_data%>%
  ggplot(aes(y=price,x=diamond_length))+
  geom_point()+
  ggtite

my_data%>%
  ggplot(aes(y=price,x=y))+
  geom_point()
my_data%>%
  ggplot(aes(y=price,x=z))+
  geom_point()


my_data%>%
  ggplot(aes(y=price,x=table))+
  geom_point()
my_data%>%
  ggplot(aes(y=price,x=color,fill=color))+
  geom_boxplot()




my_data_tranformed%>%
  ggplot(aes(y=price,x=x))+
  geom_point()

my_data_tranformed%>%
  ggplot(aes(y=price,x=y))+
  geom_point()
my_data_tranformed%>%
  ggplot(aes(y=price,x=z))+
  geom_point()


my_data_tranformed%>%
  ggplot(aes(y=price,x=table))+
  geom_point()
my_data_tranformed%>%
  ggplot(aes(y=price,x=color,fill=color))+
  geom_boxplot()

summary(my_data)
my_data=my_data%>%
  mutate(color=as.factor(color),
         cut=as.factor(cut))
my_data_tranformed<-my_data%>%
  mutate(price=log(price))

head(my_data_transformed)
my_data_tranformed%>%
  ggplot(aes(y=price,x=color,fill=color))+
  geom_boxplot()+
  ggtitle('Color and price')+
  xlab('log')

data_split=initial_split(my_data_tranformed,prop=0.8,strata=price)
train_data=training(data_split)
test_data=testing(data_split)
my_folds <- vfold_cv(train_data, v = 5,  strata = price)


my_recipe=recipe(price~.,data=train_data)%>%
  step_normalize(all_numeric_predictors())%>%
  step_corr(all_numeric_predictors())%>%
  step_other(all_nominal_predictors(),threshold = 0.01)



my_recipe%>%prep()%>%bake(new_data = NULL)
rf_model <- rand_forest(mtry = tune(),
                          min_n = tune(),
                          trees = 500) %>%
  set_mode("regression") %>%
  set_engine("ranger", importance = "impurity")
wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(rf_model)
my_grid <- grid_regular(mtry(c(1,5)),
                          min_n(),
                          levels = 5)
doParallel::registerDoParallel()
rf_tune <- tune_grid(wf,
                       resamples = my_folds,
                       grid = my_grid)

write_rds(rf_tune,'rf_tune.rds')
rf_tune=read_rds('rf_tune.rds')
rf_tune%>%autoplot()
show_best(rf_tune, metric = 'rsq')
select_best(rf_tune, metric = "rsq")
wf <- wf %>%
  finalize_workflow(select_best(rf_tune, metric = "rsq"))
test_fit <- wf %>% last_fit(split = data_split)

test_fit %>% collect_metrics()
test_data
test_fit %>% collect_predictions() %>%
  ggplot(aes(price, .pred)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(intercept = 0, slope = 1)
?diamonds
final_fit=wf%>%
  fit(train_data)
final_fit %>% extract_fit_parsnip() %>% vip()+
  ggtitle('Variable importance')
?ranger
?vip
?step_corr
set.seed(1)
madeup_data=tibble(carat=runif(2,0.2,1.04),
                   cut=c('Fair','Very Good'),
                   color=c('D', 'F'),
                   depth=runif(2,43,79),
                   diamond_depth=runif(2,2.91,4.04),
                   diamond_width=runif(2,4.73,6.54),
                   diamond_length=runif(2,4.72,6.54),
                   table=runif(2,56,59)
                   )
as.list(final_fit%>%
  predict(new_data = madeup_data))%>%
  lapply(exp)
lapply(predictions,exp)
exp(predict[1])
View(my_data)
