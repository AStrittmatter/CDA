
########################  Load Packages  ########################

# List of required packages
pkgs <- c('fBasics', 'corrplot', 'psych', 'glmnet', 'glmnetUtils', 'rpart',
          'rpart.plot', 'treeClust', 'randomForest', 'rlang', 'readr', 'devtools',
          'tidyverse', 'grf', 'reshape2', 'caret', 'neuralnet', 'plotmo', 'dmlmt',
           'doParallel', 'RandomFieldsUtils', 'doSNOW', 'rms')


# Load packages
for(pkg in pkgs){
    library(pkg, character.only = TRUE)
}

print('All packages successfully installed and loaded.')

########################  Load Data Frame  ########################

set.seed(100239) # set starting value for random number generator

# Load data frame
data_raw <- read.csv("Data/used_cars.csv",header=TRUE, sep=",")

# Outcome Variables
outcomes <- c("first_price", "final_price", "overprice")

# Covariates/Features
baseline_covariates_bin <- c("bmw_320", "opel_astra", "mercedes_c", "vw_golf", "vw_passat", 
                          "diesel",   "private_seller", "guarantee", "maintenance_cert",  "pm_green") # binary
baseline_covariates_cont <- c("mileage", "age_car_years", "other_car_owner", "inspection",
                              "co2_em", "euro_norm") # continuous/ordered discrete
baseline_covariates <- c(baseline_covariates_cont,baseline_covariates_bin)
lasso_covariates_bin <- c("mile_20", "mile_30", "mile_40", "mile_50", "mile_100", "mile_150", 
                       "age_3", "age_6","dur_next_ins_0", "dur_next_ins_1_2", "new_inspection",
                       "euro_1", "euro_2", "euro_3", "euro_4", "euro_5", "euro_6") # binary 
lasso_covariates_cont <- c("mileage2", "mileage3", "mileage4", "age_car_years2", "age_car_years3",
                           "age_car_years4") # continuous

text <- c("page_title")

lasso_covariates <- c(lasso_covariates_cont, lasso_covariates_bin)  
all_covariates <- c(baseline_covariates, lasso_covariates)
all_variables <- c(outcomes, baseline_covariates, lasso_covariates)

# Selection of Subsample size, max. 104,719 observations
# Select smaller subsample to decrease computation time
n_obs <- 10000
df <- data_raw %>%
  dplyr::sample_n(n_obs) %>%
  dplyr::select(all_variables)

print('Data frame successfully loaded and sample selected.')

########################  Table with Descriptive Statistics  ########################

desc <- fBasics::basicStats(df) %>% t() %>% as.data.frame() %>% 
  select(Mean, Stdev, Minimum, Maximum, nobs)
print(round(desc, digits=1))

# Print as tex-file
#kable(desc, "latex", booktabs = T)

########################  Correlation Matrix  ########################

corr = cor(df)
corrplot(corr, type = "upper", tl.col = "black")

# Save correlation matrix as png-file
png(height=1200, width=1200, file="correlation.png")
    corrplot(corr, type = "upper", tl.col = "black")
dev.off()

########################  Extract Dataset  ########################

# Extracting continuous variables
baseline_covariates_cont <- df %>%
  dplyr::select(baseline_covariates_cont) 

lasso_covariates_cont <- df %>%
  dplyr::select(lasso_covariates_cont) 

# Extracting indicator variables
baseline_covariates_bin <- df %>%
  dplyr::select(baseline_covariates_bin)

lasso_covariates_bin <- df %>%
  dplyr::select(lasso_covariates_bin)

# Extracting outcome 
outcomes <- df %>% dplyr::select(outcomes)

# Extracting text
text <- data_raw %>% dplyr::select(text)
print('Title of car offers')
head(text, n= 10)

# Setting up the data, renaming columns and discarding rows with NA (if any)
df <- bind_cols(outcomes, baseline_covariates_cont, baseline_covariates_bin, lasso_covariates_cont, lasso_covariates_bin) %>%
  na.omit()

print('Data successfully extracted.')

########################  Take Hold-Out-Sample  ########################

df_part <- modelr::resample_partition(df, c(obs = 0.8, hold_out = 0.2))
df_obs <- as.data.frame(df_part$obs) # Training and estimation sample
df_hold_out <- as.data.frame(df_part$hold_out) # Hold-out-sample

# Outcomes
first_price_obs <- as.matrix(df_obs[,1])
final_price_obs <- as.matrix(df_obs[,2])
overprice_obs <- as.matrix(df_obs[,3])

first_price_hold_out <- as.matrix(df_hold_out[,1])
final_price_hold_out <- as.matrix(df_hold_out[,2])
overprice_hold_out <- as.matrix(df_hold_out[,3])

## Covariates/Features
baseline_covariates_cont_obs <- as.matrix(df_obs[,c(4:9)])
baseline_covariates_bin_obs <- as.matrix(df_obs[,c(10:19)])
baseline_covariates_hold_cont_out <- as.matrix(df_hold_out[,c(4:9)])
baseline_covariates_hold_bin_out <- as.matrix(df_hold_out[,c(10:19)])

# Standardise continuous covariates
preProcValues <- preProcess(baseline_covariates_cont_obs, method = c("center", "scale")) # Take means and standard deviations from training sample
ObsTransformed <- predict(preProcValues, baseline_covariates_cont_obs) # Apply the transformation to trainings sample
HoldOutTransformed <- predict(preProcValues, baseline_covariates_hold_cont_out) # Apply the transformation to hold-out-sample (based on means and standard deviations from training sample)
# Note: Outcome variables are not rescaled

baseline_covariates_obs <- as.matrix(cbind(ObsTransformed,baseline_covariates_bin_obs)) 
baseline_covariates_hold_out <- as.matrix(cbind(HoldOutTransformed,baseline_covariates_hold_bin_out)) 
                  
print('The data is now ready for your first analysis!')

########################  Build Trees with different Leave Sizes  ########################                         

# Prepare data for tree estimator
tree_data_obs <-  data.frame(final_price_obs, baseline_covariates_obs)
empty <- as.matrix(final_price_hold_out)
empty[,1] <-NA
tree_data_hold_out <- data.frame(rbind(final_price_obs,empty),rbind(baseline_covariates_obs, baseline_covariates_hold_out))

# Setup the formula of the linear regression model
sumx <- paste(baseline_covariates, collapse = " + ")  
linear <- paste("final_price_obs",paste(sumx, sep=" + "), sep=" ~ ")
linear <- as.formula(linear)

# Build the tree
linear.singletree_1 <- rpart(formula = linear, data = tree_data_obs , method = "anova", xval = 10,
                             y = TRUE, control = rpart.control(cp = 0.00002, minbucket=500))
# Note: 'minbucket=500' imposes the restriction that each terminal leave should contain at least 500 used cars. Algorithm 'rpart' stops growing trees when either one leave has less than 500 observations or the MSE gain of addidng one addidtional leave is below cp=0.00002.

print('Relative CV-MSE for different tree sizes')
print(linear.singletree_1$cptable)

# Plot CV-MSE
plotcp(linear.singletree_1)

# Save CV-MSE as png-file
png(filename= "cp_tree1.png", units="in", width=5, height=4, pointsize=12, res=72)
    plotcp(linear.singletree_1)
dev.off()

########################  Select the Tree that Minimises CV-MSE  ######################## 

op.index_1 <- which.min(linear.singletree_1$cptable[, "xerror"])
print(paste0("Optimal number final leaves: ", op.index_1))

# Get cp-value that corresponds to optimal tree size
cp.vals_1 <- linear.singletree_1$cptable[op.index_1, "CP"]

########################  Select the Optimal Tree and Assess Out-of-Sample Performance  ######################## 

# Prune the tree
treepruned.linearsingle_1 <- prune(linear.singletree_1, cp = cp.vals_1)

# Predict final price in the observed and hold-out-samples
pred_tree_hold_out_1 <- as.matrix(predict(treepruned.linearsingle_1, newdata=tree_data_hold_out))
pred_tree_obs_1 <- pred_tree_hold_out_1[c(1:nrow(tree_data_obs)),]
r <-nrow(final_price_obs)+1
pred_tree_hold_out_1 <- pred_tree_hold_out_1[c(r:nrow(pred_tree_hold_out_1)),]

## Assess performance of tree estimator
# In-sample RMSE
rmse_obs_1 <- round(sqrt(mean((final_price_obs - pred_tree_obs_1)^2)),digits=3)
# Hold-out-sample RMSE
rmse_hold_out_1 <- round(sqrt(mean((final_price_hold_out - pred_tree_hold_out_1)^2)),digits=3)
# In-sample R-squared
r2_obs_1 <- round(1-mean((final_price_obs - pred_tree_obs_1)^2)/mean((final_price_obs - mean(final_price_obs))^2),digits=3)
# Hold-out-sample R-squared
r2_hold_out_1 <- round(1-mean((final_price_hold_out - pred_tree_hold_out_1)^2)/mean((final_price_hold_out - mean(final_price_hold_out))^2),digits=3)

print(paste0("In-Sample RMSE: ", rmse_obs_1))
print(paste0("Hold-out-Sample RMSE: ", rmse_hold_out_1))
print(paste0("In-Sample R-squared: ", r2_obs_1))
print(paste0("Hold-out-Sample R-squared: ", r2_hold_out_1))

########################  Visulatisation of tree  ######################## 

## Plot tree structure
rpart.plot(treepruned.linearsingle_1,digits=3)
# Note: All continuous variables are standardised.

# Save tree structure as png-file
png(filename= "full_tree1.png",units="in", width=9, height=9, pointsize=12,res=72)
    rpart.plot(treepruned.linearsingle_1,digits=3)
dev.off()


######################## Average Value of Each Covariate by Leaf  ######################## 
## Code from Susan Athey and Guido Imbens AEA lecture

# Take hold-out data only
tree_data_out <- data.frame(pred_tree_hold_out_1, baseline_covariates_hold_out)

# Map to each individual row the leaf number and add the covariates
individual_leaf <- treeClust::rpart.predict.leaves(treepruned.linearsingle_1, tree_data_out)  %>% 
  as_tibble()  %>% 
  dplyr::rename(leaf=value) 
leaf_covariates <- cbind(individual_leaf, tree_data_out[baseline_covariates])

# Get predicted final price of each leaf 
leaf_price <- treepruned.linearsingle_1$frame %>% as_tibble() %>%
  dplyr::mutate(row = 1:nrow(.)) %>% 
  dplyr::filter(var == "<leaf>") %>% 
  dplyr::rename(leaf=row, pred_price=yval) %>% 
  dplyr::select(leaf, pred_price) 

# Merge all the information on leaf level
leaf_data <- left_join(leaf_covariates, leaf_price, by="leaf")

# Mean of each covariate on each leaf, 
# Leafs sorted and renumbered by predicted prive
leaf_mean <- leaf_data %>% 
  dplyr::group_by(leaf) %>%
  dplyr::summarise_all(mean) %>%
  dplyr::arrange(desc(pred_price)) %>%
  dplyr::mutate(leaf = 1:nrow(.)) 

# Plot
plt <- leaf_mean %>% 
  dplyr::select(leaf, baseline_covariates[c(16:1)]) %>%
  melt(id="leaf") %>%
  ggplot(aes(x=factor(leaf), y=variable, fill=value)) +
  geom_raster() +
  scale_fill_gradient2() + 
  scale_x_discrete(breaks=seq_along(leaf_mean$pred_price),      
                   labels=round(leaf_mean$pred_price, 1)) +
  # From here on, all the code is optional styling
  geom_tile(colour="white",size=0.25) +            # white cell border
  labs(x="Predicted final price",
       y="", title="Average covariate value by leaf") +# axis labels 
  coord_fixed()+                                   # square cells
  theme_grey(base_size=8)+                         # basic hue 
  theme(
    axis.text=element_text(face="bold"),      # axis font style
    plot.background=element_blank(),          # cleaner background
    panel.border=element_blank(),             # cleaner panel
    legend.key.width=grid::unit(0.2,"cm"),    # slim legend color bar
    axis.ticks=element_line(size=0.4),        # tick style
    axis.text.x=element_text(size=7,          # tick label style
                             colour="grey40",
                             angle = 60,
                             hjust = 1),
    plot.title=element_text(colour="grey40",  # plot title style
                            hjust=.5,size=7,
                            face="bold")
  )

plot(plt)

# Save average covariate values as png-file
png(filename= "regressor_values.png",units="in", width=6, height=6, pointsize=38,res=300)
    plt
dev.off()

######################## Deep tree estimator  ########################                         

# Build deep Tree
linear.singletree_2 <- rpart(formula = linear, data = tree_data_obs , method = "anova", xval = 10,
                             y = TRUE, control = rpart.control(cp = 0.00002, minbucket=5))

# Find tree size that minimises CV-MSE
op.index_2 <- which.min(linear.singletree_2$cptable[, "xerror"])

# Plot CV-MSE
plotcp(linear.singletree_2)
abline(v = op.index_2, lty = "dashed")


# Get cp-value that corresponds to optimal tree size
cp.vals_2 <- linear.singletree_2$cptable[op.index_2, "CP"]

# Prune the tree
treepruned.linearsingle_2 <- prune(linear.singletree_2, cp = cp.vals_2)

# Plot tree structure
rpart.plot(treepruned.linearsingle_2,digits=3)
treepruned.linearsingle_2_short <- prune(linear.singletree_2, cp = 150*cp.vals_2)
rpart.plot(treepruned.linearsingle_2_short,digits=3, main = "First few leaves",fallen.leaves=FALSE)


# Predict final price in the observed and hold-out-samples
pred_tree_hold_out_2 <- as.matrix(predict(treepruned.linearsingle_2, newdata=tree_data_hold_out))
pred_tree_obs_2 <- pred_tree_hold_out_2[c(1:nrow(tree_data_obs)),]
r <-nrow(final_price_obs)+1
pred_tree_hold_out_2 <- pred_tree_hold_out_2[c(r:nrow(pred_tree_hold_out_2)),]

## Assess performance of tree estimator
# In-sample RMSE
rmse_obs_2 <- round(sqrt(mean((final_price_obs - pred_tree_obs_2)^2)),digits=3)
# Hold-out-sample RMSE
rmse_hold_out_2 <- round(sqrt(mean((final_price_hold_out - pred_tree_hold_out_2)^2)),digits=3)
# In-sample R-squared
r2_obs_2 <- round(1-mean((final_price_obs - pred_tree_obs_2)^2)/mean((final_price_obs - mean(final_price_obs))^2),digits=3)
# Hold-out-sample R-squared
r2_hold_out_2 <- round(1-mean((final_price_hold_out - pred_tree_hold_out_2)^2)/mean((final_price_hold_out - mean(final_price_hold_out))^2),digits=3)

print(paste0("In-Sample RMSE: ", rmse_obs_2))
print(paste0("Hold-out-Sample RMSE: ", rmse_hold_out_2))
print(paste0("In-Sample R-squared: ", r2_obs_2))
print(paste0("Hold-out-Sample R-squared: ", r2_hold_out_2))

######################## Honest deep tree estimator ########################                        

# Create tratining and estimation sample
df_obs_part <- modelr::resample_partition(df_obs, c(train = 0.5, est = 0.5))
df_train <- as.data.frame(df_obs_part$train)
df_est <- as.data.frame(df_obs_part$est)

# Outcomes
final_price_train <- as.matrix(df_train[,2])
final_price_est <- as.matrix(df_est[,2])

# Covariates/Features
baseline_covariates_hold_cont_out <- as.matrix(df_hold_out[,c(4:9)])
baseline_covariates_cont_train <- as.matrix(df_train[,c(4:9)]) 
baseline_covariates_cont_est <- as.matrix(df_est[,c(4:9)])
baseline_covariates_hold_bin_out <- as.matrix(df_hold_out[,c(10:19)])
baseline_covariates_bin_train <- as.matrix(df_train[,c(10:19)]) 
baseline_covariates_bin_est <- as.matrix(df_est[,c(10:19)])

# Scale continuous variables
preProcValues <- preProcess(baseline_covariates_cont_train, method = c("center", "scale"))
TrainTransformed <- predict(preProcValues, baseline_covariates_cont_train)
HoldOutTransformed <- predict(preProcValues, baseline_covariates_hold_cont_out)
EstTransformed <- predict(preProcValues, baseline_covariates_cont_est)

baseline_covariates_hold_out <- as.matrix(cbind(HoldOutTransformed,baseline_covariates_hold_bin_out)) 
baseline_covariates_train <- as.matrix(cbind(TrainTransformed,baseline_covariates_bin_train)) 
baseline_covariates_est <- as.matrix(cbind(EstTransformed,baseline_covariates_bin_est)) 

# Prepare data for tree estimator
tree_data_train <-  data.frame(final_price_train, baseline_covariates_train)
empty1 <- as.matrix(final_price_train)
empty1[,1] <-NA
empty2 <- as.matrix(final_price_hold_out)
empty2[,1] <-NA
tree_data_hold_out <-  data.frame(rbind(empty1,final_price_est,empty2),rbind(baseline_covariates_train,baseline_covariates_est, baseline_covariates_hold_out))

# Setup the formula of the linear regression model
linear <- paste("final_price_train",paste(sumx, sep=" + "), sep=" ~ ")
linear <- as.formula(linear)

# Build deep tree
linear.singletree_3 <- rpart(formula = linear, data = tree_data_train , method = "anova", xval = 10,
                             y = TRUE, control = rpart.control(cp = 0.00002, minbucket=5))


# Find tree size that minimises CV-MSE
op.index_3 <- which.min(linear.singletree_3$cptable[, "xerror"])

# Plot CV-MSE
plotcp(linear.singletree_3)
abline(v = op.index_3, lty = "dashed")

# Get cp-value that corresponds to optimal tree size
cp.vals_3 <- linear.singletree_3$cptable[op.index_3, "CP"]

# Prune the tree
treepruned.linearsingle_3 <- prune(linear.singletree_3, cp = cp.vals_3)

# Predict final price in the observed and hold-out-samples
pred_tree_hold_out_3 <- as.matrix(predict(treepruned.linearsingle_3, newdata=tree_data_hold_out))
pred_tree_train_3 <- pred_tree_hold_out_3[c(1:nrow(final_price_train)),]
r <-nrow(final_price_train)+1
pred_tree_est_3 <- pred_tree_hold_out_3[c(r:nrow(final_price_obs)),]
r <-nrow(final_price_obs)+1
pred_tree_hold_out_3 <- pred_tree_hold_out_3[c(r:nrow(pred_tree_hold_out_3)),]

## Assess performance of tree estimator
# Training-sample RMSE
rmse_train_3 <- round(sqrt(mean((final_price_train - pred_tree_train_3)^2)),digits=3)
# Estimation-sample RMSE
rmse_est_3 <- round(sqrt(mean((final_price_est - pred_tree_est_3)^2)),digits=3)
# Hold-out-sample RMSE
rmse_hold_out_3 <- round(sqrt(mean((final_price_hold_out - pred_tree_hold_out_3)^2)),digits=3)
# Training-sample R-squared
r2_train_3 <- round(1-mean((final_price_train - pred_tree_train_3)^2)/mean((final_price_train - mean(final_price_train))^2),digits=3)
# Estimation-sample R-squared
r2_est_3 <- round(1-mean((final_price_est - pred_tree_est_3)^2)/mean((final_price_est - mean(final_price_est))^2),digits=3)
# Hold-out-sample R-squared
r2_hold_out_3 <- round(1-mean((final_price_hold_out - pred_tree_hold_out_3)^2)/mean((final_price_hold_out - mean(final_price_hold_out))^2),digits=3)

print(paste0("Training-Sample RMSE: ", rmse_train_3))
print(paste0("Estimation-Sample RMSE: ", rmse_est_3))
print(paste0("Hold-out-Sample RMSE: ", rmse_hold_out_3))
print(paste0("Training-Sample R-squared: ", r2_train_3))
print(paste0("Estimation-Sample R-squared: ", r2_est_3))
print(paste0("Hold-out-Sample R-squared: ", r2_hold_out_3))


########################  Crosfitted honest deep tree estimator  ########################                        

# Alternate tratining and estimation sample
tree_data_train1 <-  data.frame(final_price_train, baseline_covariates_train)
tree_data_train2 <-  data.frame(final_price_est, baseline_covariates_est)

# Prepare both data data for tree estimator
empty1 <- as.matrix(final_price_train)
empty1[1,] <-NA
empty2 <- as.matrix(final_price_hold_out)
empty2[1,] <-NA
tree_data_hold_out1 <-  data.frame(rbind(empty1,final_price_est,empty2),rbind(baseline_covariates_train,baseline_covariates_est, baseline_covariates_hold_out))

empty1 <- as.matrix(final_price_est)
empty1[1,] <-NA
empty2 <- as.matrix(final_price_hold_out)
empty2[1,] <-NA
tree_data_hold_out2 <-  data.frame(rbind(final_price_train,empty1,empty2),rbind(baseline_covariates_train,baseline_covariates_est, baseline_covariates_hold_out))

#####

# Setup the formula of the linear regression model for the first tree
linear <- paste("final_price_train",paste(sumx, sep=" + "), sep=" ~ ")
linear <- as.formula(linear)

# Build first deep tree
linear.singletree_4 <- rpart(formula = linear, data = tree_data_train1 , method = "anova", xval = 10,
                             y = TRUE, control = rpart.control(cp = 0.00002, minbucket=5))

# Find tree size that minimises CV-MSE
op.index_4 <- which.min(linear.singletree_4$cptable[, "xerror"])

# Plot CV-MSE
plotcp(linear.singletree_4)
abline(v = op.index_4, lty = "dashed")

# Get cp-value that corresponds to optimal tree size
cp.vals_4 <- linear.singletree_4$cptable[op.index_4, "CP"]

# Prune the tree
treepruned.linearsingle_4 <- prune(linear.singletree_4, cp = cp.vals_4)

# Predict final price in the hold-out-sample
pred_tree_hold_out_4 <- as.matrix(predict(treepruned.linearsingle_4, newdata=tree_data_hold_out1))
r <-nrow(final_price_obs)+1
pred_tree_hold_out_4 <- pred_tree_hold_out_4[c(r:nrow(pred_tree_hold_out_4)),]

#####

# Setup the formula of the linear regression model for the second tree
linear <- paste("final_price_est",paste(sumx, sep=" + "), sep=" ~ ")
linear <- as.formula(linear)

# Build second deep tree
linear.singletree_5 <- rpart(formula = linear, data = tree_data_train2 , method = "anova", xval = 10,
                             y = TRUE, control = rpart.control(cp = 0.00000001, minbucket=5))

# Find tree size that minimises CV-MSE
op.index_5 <- which.min(linear.singletree_5$cptable[, "xerror"])

# Plot CV-MSE
plotcp(linear.singletree_5)
abline(v = op.index_5, lty = "dashed")

# Get cp-value that corresponds to optimal tree size
cp.vals_5 <- linear.singletree_5$cptable[op.index_5, "CP"]

# Prune the tree
treepruned.linearsingle_5 <- prune(linear.singletree_5, cp = cp.vals_5)

# Predict final price in the hold-out-sample
pred_tree_hold_out_5 <- as.matrix(predict(treepruned.linearsingle_5, newdata=tree_data_hold_out2))
r <-nrow(final_price_obs)+1
pred_tree_hold_out_5 <- pred_tree_hold_out_5[c(r:nrow(pred_tree_hold_out_5)),]

#####

## Assess performance of tree estimator
# Hold-out-sample RMSE
rmse_hold_out_4 <- round(sqrt(mean((final_price_hold_out - 0.5*(pred_tree_hold_out_4 + pred_tree_hold_out_5))^2)),digits=3)
# Hold-out-sample R-squared
r2_hold_out_4 <- round(1-mean((final_price_hold_out - 0.5*(pred_tree_hold_out_4 + pred_tree_hold_out_5))^2)/mean((final_price_hold_out - mean(final_price_hold_out))^2),digits=3)

print(paste0("Hold-out-Sample RMSE: ", rmse_hold_out_4))
print(paste0("Hold-out-Sample R-squared: ", r2_hold_out_4))

########################  Random forest estimator  ######################## 

# Tuning parameters
min_tree = 1
num_trees = 250 # Use at least 1,000 trees
cov_frac = 2/3
sample_part= 0.5

# Build random forest
# grf-package from Athey, Tibshirani, and Wager (2018)
forest <- regression_forest(baseline_covariates_obs, final_price_obs, sample.fraction = sample_part, 
                  mtry = floor(cov_frac*ncol(baseline_covariates_obs)),
                  num.trees = num_trees, min.node.size = min_tree,
                  honesty = TRUE, honesty.fraction = 0.5)

# Predict prices in hold-out-sample
pred_forest <- predict(forest, newdata = baseline_covariates_hold_out)

## Assess performance of forest estimator
# Hold-out-sample RMSE
rmse_forest <- round(sqrt(mean((final_price_hold_out - pred_forest$predictions)^2)),digits=3)
# Hold-out-sample R-squared
r2_forest <- round(1-mean((final_price_hold_out - pred_forest$predictions)^2)/mean((final_price_hold_out - mean(final_price_hold_out))^2),digits=3)

print(paste0("Hold-out-Sample RMSE: ", rmse_forest))
print(paste0("Hold-out-Sample R-squared: ", r2_forest))


########################  Number of splits by covariate and tree depht  ######################## 

split <- split_frequencies(forest, max.depth = 4)
colnames(split) <- baseline_covariates
print(split)

########################  Select tuning parameters for forest  ######################## 

#for_sizes = c(1,5, 10,15, 20,25,50,100,150,200,250, 300,350, 400, 450, 500, 1000, 1500, 2000, 3000, 4000, 5000, 10000)
for_sizes = c(1,5, 10,15, 20,25,50,100,150,200,250, 300,350, 400, 450, 500) # Because of computation time we consider only forests with 500 trees. At home you can consider larger forests.
auc <- matrix(NA,nrow=length(for_sizes),ncol=3)
ctr <- 0
for (n in for_sizes){
  ctr <- ctr + 1
  auc[ctr,1] <- n
  
  forest <- regression_forest(baseline_covariates_obs, final_price_obs, sample.fraction = 0.5, 
                              mtry = floor(cov_frac*ncol(baseline_covariates_obs)),
                              num.trees = n, min.node.size = min_tree,
                              honesty = TRUE, honesty.fraction = 0.5)
  
  # Predict prices in hold-out-sample
  pred_forest <- predict(forest, newdata = baseline_covariates_hold_out)
  rmse_forest <- round(sqrt(mean((final_price_hold_out - pred_forest$predictions)^2)),digits=3)
  auc[ctr,2] <- rmse_forest
  if (ctr >1) {
    auc[ctr,3] <- rmse_forest-auc[ctr-1,2]
  }
}

plot(auc[,1], auc[,2], main="Tuning of forest size", xlab="Number of trees in forest ", ylab="RMSE", pch=19)
nls_fit <- lm(auc[,2] ~  auc[,1] + I(auc[,1]^(1/2)) + I(auc[,1]^2) + I(auc[,1]^3) + I(log(auc[,1])))
lines(auc[,1], predict(nls_fit), col = "red")

plot(auc[c(2:nrow(auc)),1], auc[c(2:nrow(auc)),3], main="Tuning of forest size", xlab="Number of trees in forest ", ylab="Delta RMSE", pch=19)
nls_fit <- lm(auc[c(2:nrow(auc)),3] ~  auc[c(2:nrow(auc)),1] + I(auc[c(2:nrow(auc)),1]^(1/2)) + I(auc[c(2:nrow(auc)),1]^2) + I(auc[c(2:nrow(auc)),1]^3) + I(log(auc[c(2:nrow(auc)),1])))
lines(auc[c(2:nrow(auc)),1], predict(nls_fit), col = "red")
abline(h=0)


# Save graph
png(filename= "auc.png",units="in", width=6, height=6, pointsize=10,res=300)
  plot(auc[,1], auc[,2], main="Tuning of forest size", xlab="Number of trees in forest ", ylab="RMSE", pch=19)
  nls_fit <- lm(auc[,2] ~  auc[,1] + I(auc[,1]^(1/2)) + I(auc[,1]^2) + I(auc[,1]^3) + I(log(auc[,1])))
  lines(auc[,1], predict(nls_fit), col = "red")
dev.off()

png(filename= "delta_auc.png",units="in", width=6, height=6, pointsize=10,res=300)
  plot(auc[c(2:nrow(auc)),1], auc[c(2:nrow(auc)),3], main="Tuning of forest size", xlab="Number of trees in forest ", ylab="Delta RMSE", pch=19)
  nls_fit <- lm(auc[c(2:nrow(auc)),3] ~  auc[c(2:nrow(auc)),1] + I(auc[c(2:nrow(auc)),1]^(1/2)) + I(auc[c(2:nrow(auc)),1]^2) + I(auc[c(2:nrow(auc)),1]^3) + I(log(auc[c(2:nrow(auc)),1])))
  lines(auc[c(2:nrow(auc)),1], predict(nls_fit), col = "red")
  abline(h=0)
dev.off()

########################  Prepare Lasso data  ######################## 


# Generate some noisy covariates to disturbe the estimation
noise.covars.hold.out <- matrix(data = rnorm(nrow(df_hold_out)),  nrow = nrow(df_hold_out), ncol = 13)
colnames(noise.covars.hold.out) <- c("noise1", "noise2", "noise3", "noise4", "noise5", "noise6", "noise7", "noise8", "noise9", "noise10", "noise11", "noise12","noise13")

noise.covars.est <- matrix(data = rnorm(nrow(df_est)),  nrow = nrow(df_est), ncol = 13)
colnames(noise.covars.est) <- c("noise1", "noise2", "noise3", "noise4", "noise5", "noise6", "noise7", "noise8", "noise9", "noise10", "noise11", "noise12","noise13")

noise.covars.train <- matrix(data = rnorm(nrow(df_train)),  nrow = nrow(df_train), ncol = 13)
colnames(noise.covars.train) <- c("noise1", "noise2", "noise3", "noise4", "noise5", "noise6", "noise7", "noise8", "noise9", "noise10", "noise11", "noise12","noise13")


# Prepare LASSO variables
lasso_covariates_hold_out_cont <- as.matrix(df_hold_out[,c(4:6,8,20:25)])
lasso_covariates_est_cont <- as.matrix(df_est[,c(4:6,8,20:25)])
lasso_covariates_train_cont <- as.matrix(df_train[,c(4:6,8,20:25)])

# Scale continuous variables
preProcValuesLasso <- preProcess(lasso_covariates_train_cont, method = c("center", "scale"))
TrainTransformed <- predict(preProcValuesLasso, lasso_covariates_train_cont)
HoldOutTransformed <- predict(preProcValuesLasso, lasso_covariates_hold_out_cont)
EstTransformed <- predict(preProcValuesLasso, lasso_covariates_est_cont)

lasso_covariates_hold_out <- as.matrix(cbind(HoldOutTransformed,df_hold_out[,c(10:19, 26:ncol(df_hold_out))],noise.covars.hold.out)) 
lasso_covariates_train <- as.matrix(cbind(TrainTransformed,df_train[,c(10:19,26:ncol(df_train))],noise.covars.train)) 
lasso_covariates_est <- as.matrix(cbind(EstTransformed,df_est[,c(10:19,26:ncol(df_est))],noise.covars.est)) 

#########################
print('Lasso data ready')

########################  CV-LASSO  ######################## 
p = 1 # 1 for LASSO, 0 for Ridge

lasso.linear <- cv.glmnet(lasso_covariates_train, final_price_train, alpha=p, type.measure = 'mse', parallel=FALSE)
plot(lasso.linear)

print(paste0("Lambda minimising CV-MSE: ", round(lasso.linear$lambda.min,digits=8)))
# 1 standard error rule reduces the number of included covariates
print(paste0("Lambda 1 standard error rule: ", round(lasso.linear$lambda.1se,digits=8)))

########################  Vissualisation of LASSO  ######################## 

lambda_min =  lasso.linear$glmnet.fit$lambda[50]/lasso.linear$glmnet.fit$lambda[1]
mod <- glmnet(lasso_covariates_train , final_price_train,lambda.min = lambda_min,  alpha=p)
glmcoef<-coef(mod,lasso.linear$lambda.1se)
coef.increase<-dimnames(glmcoef[glmcoef[,1]>0,0])[[1]]
coef.decrease<-dimnames(glmcoef[glmcoef[,1]<0,0])[[1]]

maxcoef<-coef(mod,s=lambda_min)
coef<-dimnames(maxcoef[maxcoef[,1]!=0,0])[[1]]
allnames<-dimnames(maxcoef[maxcoef[,1]!=0,0])[[1]][order(maxcoef[maxcoef[,1]!=0,ncol(maxcoef)],decreasing=TRUE)]
allnames<-setdiff(allnames,allnames[grep("Intercept",allnames)])

#assign colors
cols<-rep("gray",length(allnames))
cols[allnames %in% coef.increase]<-"red"      # higher mpg is good
cols[allnames %in% coef.decrease]<- "green"        # lower mpg is not

plot_glmnet(mod,label=TRUE,s=lasso.linear$lambda.1se,col= cols)

########################  Plot LASSO Coefficients  ########################

print('LASSO coefficients')
print(glmcoef)
# the LASSO coefficients are biased because of the penalty term

########################  Performance of LASSO  ######################## 

# Estimate LASSO model with 1 standard error lambda in estimation sample
lasso.fit <- glmnet(lasso_covariates_est , final_price_est,lambda = lasso.linear$lambda.1se)

# Extrapolate LASSO estimates to hold-out-sample
yhat.lasso <- predict(lasso.fit, lasso_covariates_hold_out)

## Assess performance of LASSO estimator
# Hold-out-sample RMSE
rmse_lasso <- round(sqrt(mean((final_price_hold_out - yhat.lasso)^2)),digits=3)
# Hold-out-sample R-squared
r2_lasso <- round(1-mean((final_price_hold_out - yhat.lasso)^2)/mean((final_price_hold_out - mean(final_price_hold_out))^2),digits=3)

print(paste0("Hold-out-Sample RMSE: ", rmse_lasso))
print(paste0("Hold-out-Sample R-squared: ", r2_lasso))

########################  Post-LASSO estimator  ######################## 

# Estimate LASSO model with 1 standard error lambda in training sample
lasso.linear.fit <- glmnet(lasso_covariates_train , final_price_train,lambda = lasso.linear$lambda.1se)

# Select covariates with non-zero coefficients
coef <- predict(lasso.linear.fit, type = "nonzero") # Method 2
colnames <- colnames(lasso_covariates_train)
selected.vars <- colnames[unlist(coef)]

# Prepare (unscaled) LASSO variables
lasso_covariates_hold_out <- as.data.frame(cbind(df_hold_out[,c(4:ncol(df_hold_out))],noise.covars.hold.out)) 
lasso_covariates_est <- as.data.frame(cbind(df_est[,c(4:ncol(df_est))],noise.covars.est)) 

# Linear Post-LASSO fit in estimation sample
post.lasso.model <- paste("final_price_est", paste(selected.vars,collapse=" + "),  sep = " ~ ") 
post.lasso.model <- as.formula(post.lasso.model)
post.lasso <- lm(post.lasso.model, data=lasso_covariates_est)
summary(post.lasso)

# Extrapolate Post-LASSO estimates to hold-out-sample
yhat.post.lasso <- predict(post.lasso, newdata=lasso_covariates_hold_out)

## Assess performance of LASSO estimator
# Hold-out-sample RMSE
rmse_post_lasso <- round(sqrt(mean((final_price_hold_out - yhat.post.lasso)^2)),digits=3)
# Hold-out-sample R-squared
r2_post_lasso <- round(1-mean((final_price_hold_out - yhat.post.lasso)^2)/mean((final_price_hold_out - mean(final_price_hold_out))^2),digits=3)

print(paste0("Hold-out-Sample RMSE: ", rmse_post_lasso))
print(paste0("Hold-out-Sample R-squared: ", r2_post_lasso))


########################  Post-LASSO estimator with correct CV  ######################## 
# By MCKnaus

lasso.linear <- post_lasso_cv(lasso_covariates_train, final_price_train,lambda.min = exp(-9), 
                              alpha=1, parallel=FALSE, output = T, se_rule = c(-1))
#lasso.linear$names_Xse_pl$`SE -1`[-1]

# Extrapolate Post-LASSO estimates to hold-out-sample
post.lasso.model <- paste("final_price_est", paste(lasso.linear$names_Xse_pl$`SE -1`[-1],collapse=" + "),  sep = " ~ ") 
post.lasso.model <- as.formula(post.lasso.model)
post.lasso <- lm(post.lasso.model, data=lasso_covariates_est)
print(post.lasso$coefficients)

yhat.post.lasso <- predict(post.lasso, newdata=lasso_covariates_hold_out)

## Assess performance of LASSO estimator
# Hold-out-sample RMSE
rmse_post_lasso <- round(sqrt(mean((final_price_hold_out - yhat.post.lasso)^2)),digits=3)
# Hold-out-sample R-squared
r2_post_lasso <- round(1-mean((final_price_hold_out - yhat.post.lasso)^2)/mean((final_price_hold_out - mean(final_price_hold_out))^2),digits=3)

print(paste0("Hold-out-Sample RMSE: ", rmse_post_lasso))
print(paste0("Hold-out-Sample R-squared: ", r2_post_lasso))

########################  Logit-LASSO  ######################## 

overprice_train <- as.matrix(df_train[,3])
overprice_est <- as.matrix(df_est[,3])
lasso_covariates_est <- as.matrix(lasso_covariates_est)
lasso_covariates_hold_out <- as.matrix(lasso_covariates_hold_out)

# Cross-validate Lambda of LASSO model##

lasso.logit <- cv.glmnet(lasso_covariates_train, overprice_train,alpha=1,family='binomial',type.measure = 'mse', parallel=FALSE)
plot(lasso.logit)

print(paste0("Lambda minimising CV-MSE: ", round(lasso.logit$lambda.min,digits=8)))
# 1 standard error rule reduces the number of included covariates
print(paste0("Lambda 1 standard error rule: ", round(lasso.logit$lambda.1se,digits=8)))


# Estimate LASSO model with 1 standard error lambda in estimation sample
lasso.logit.fit <- glmnet(lasso_covariates_est , overprice_est, alpha=1, family='binomial', lambda = lasso.logit$lambda.1se)
mean(overprice_est)
coef(lasso.logit.fit, s= lasso.logit$lambda.1se)

# Extrapolate LASSO estimates to hold-out-sample
yhat.lasso.logit <- predict(lasso.logit.fit, lasso_covariates_hold_out, lambda = lasso.logit$lambda.1se, type='response')

## Assess performance of LASSO estimator
# Hold-out-sample RMSE
rmse_lasso_logit <- round(sqrt(mean((overprice_hold_out - yhat.lasso.logit)^2)),digits=3)
# Hold-out-sample R-squared
r2_lasso_logit <- round(1-mean((overprice_hold_out - yhat.lasso.logit)^2)/mean((overprice_hold_out - mean(overprice_hold_out))^2),digits=3)

print(paste0("Hold-out-Sample RMSE: ", rmse_lasso_logit))
print(paste0("Hold-out-Sample R-squared: ", r2_lasso_logit))

########################  Load New Car data  ######################## 

# 26251 new cars are on the market
# Load data frame
data_new <- read.csv("Data/used_cars_on_market.csv",header=TRUE, sep=",")
df_new <- data_new %>%
  dplyr::select(c("id",all_covariates))

# Covariates/Features
baseline_covariates_bin <- c("bmw_320", "opel_astra", "mercedes_c", "vw_golf", "vw_passat", 
                             "diesel",   "private_seller", "guarantee", "maintenance_cert",  "pm_green") # binary
baseline_covariates_cont <- c("mileage", "age_car_years", "other_car_owner", "inspection",
                              "co2_em", "euro_norm") # continuous/ordered discrete
baseline_covariates <- c(baseline_covariates_cont,baseline_covariates_bin)
lasso_covariates_bin <- c("mile_20", "mile_30", "mile_40", "mile_50", "mile_100", "mile_150", 
                          "age_3", "age_6","dur_next_ins_0", "dur_next_ins_1_2", "new_inspection",
                          "euro_1", "euro_2", "euro_3", "euro_4", "euro_5", "euro_6") # binary 
lasso_covariates_cont <- c("mileage2", "mileage3", "mileage4", "age_car_years2", "age_car_years3",
                           "age_car_years4") # continuous


# Extracting continuous variables
baseline_covariates_cont_new <- df_new %>%
  dplyr::select(baseline_covariates_cont) 

lasso_covariates_cont_new <- df_new %>%
  dplyr::select(lasso_covariates_cont) 

# Extracting indicator variables
baseline_covariates_bin_new <- df_new %>%
  dplyr::select(baseline_covariates_bin)

lasso_covariates_bin_new <- df_new %>%
  dplyr::select(lasso_covariates_bin)

# Extracting outcome 
id <- data_new %>% dplyr::select("id")

# Setting up the data, renaming columns and discarding rows with NA (if any)
df_new <- bind_cols(id, baseline_covariates_cont_new, baseline_covariates_bin_new, lasso_covariates_cont_new, lasso_covariates_bin_new) %>%
  na.omit()


# Baseline Covariates
baseline_covariates_cont_train_old <- as.matrix(df_train[,c(4:9)]) 
preProcValues <- preProcess(baseline_covariates_cont_train_old , method = c("center", "scale")) # Take means and standard deviations from training sample
NewTransformed <- predict(preProcValues, baseline_covariates_cont_new)
baseline_covariates_new <- as.matrix(cbind(NewTransformed,baseline_covariates_bin_new)) 


# Lasso Covariates
l_cov_train_cont_old <- as.matrix(df_train[,c(4:6,8,20:25)])
l_cov_train_cont_new <- as.matrix( bind_cols(baseline_covariates_cont_new,lasso_covariates_cont_new))
l_cov_train_bin_new <- as.matrix( bind_cols(baseline_covariates_bin_new,lasso_covariates_bin_new))
noise.covars.new <- matrix(data = rnorm(nrow(df_new)),  nrow = nrow(df_new), ncol = 13)
colnames(noise.covars.new) <- c("noise1", "noise2", "noise3", "noise4", "noise5", "noise6", "noise7", "noise8", "noise9", "noise10", "noise11", "noise12","noise13")
preProcValues <- preProcess(l_cov_train_cont_old, method = c("center", "scale"))
NewTransL <- predict(preProcValues, l_cov_train_cont_new)
lasso_covariates_new <- as.matrix(cbind(NewTransL,l_cov_train_bin_new,noise.covars.new)) 

print('New data successfully extracted.')

########################  Put your code here  ######################## 










####################################################################### 

########################  Save predictions ######################## 

# Save a prediction for the final_price and overprice
predictions = matrix(NA,ncol=2, nrow=nrow(id))
predictions <- cbind(df_new[,1],predictions)
colnames(predictions) <- c("id","final_price", "overprice")
write.csv(predictions, file = "predictions.csv")
print('predictions saved')


