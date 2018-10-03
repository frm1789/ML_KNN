library(class)
library(gmodels)

## 1. data
df_iris <- iris

## 2. Target variable converted to factor 
table(df_iris$Species)
#    setosa versicolor  virginica 
#   50         50         50 

# recode diagnosis as a factor
df_iris$Species <- factor(df_iris$Species, levels = c("S", "V", "I"),
                         labels = c("setosa", "versicolor", "virginica"))

# table or proportions with more informative labels
round(prop.table(table(df_iris$Species)) * 100, digits = 1)

## 3. Apply scaling on numeric features
## summarize three numeric features
summary(df_iris[c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])

## 4. Mixing because this dataset is ordered seq.

set.seed(99) # required to reproduce the results
rnum<- sample(rep(1:150)) # randomly generate numbers from 1 to 150
rnum
df_iris <- df_iris[rnum,] #randomize "iris" dataset
head(iris)

df_iris <- iris

## 5. Apply scaling on numeric features
iris_train<- df_iris[1:130,-5]
iris_train_labels<- df_iris[1:130,5]

iris_test<- df_iris[131:150,-5]
iris_test_labels<- df_iris[131:150,5]
summary(iris_train)

## 6. Training a model on the data 
iris_test_pred <- knn(train = iris_train, test = iris_test,
                      cl = iris_train_labels, k=21)

## 7. Evaluating model performance: cross tabulation of predicted vs. actual
CrossTable(x = iris_test_labels, y = iris_test_pred,
           prop.chisq=FALSE)


# | iris_test_pred 
# iris_test_labels |     setosa | versicolor |  virginica |  Row Total | 
#   -----------------|------------|------------|------------|------------|
#         setosa |          5 |          0 |          0 |          5 | 
#   |      1.000 |      0.000 |      0.000 |      0.250 | 
#   |      1.000 |      0.000 |      0.000 |            | 
#   |      0.250 |      0.000 |      0.000 |            | 
#   -----------------|------------|------------|------------|------------|
#     versicolor |          0 |          7 |          1 |          8 | 
#   |      0.000 |      0.875 |      0.125 |      0.400 | 
#   |      0.000 |      1.000 |      0.125 |            | 
#   |      0.000 |      0.350 |      0.050 |            | 
#   -----------------|------------|------------|------------|------------|
#      virginica |          0 |          0 |          7 |          7 | 
#   |      0.000 |      0.000 |      1.000 |      0.350 | 
#   |      0.000 |      0.000 |      0.875 |            | 
#   |      0.000 |      0.000 |      0.350 |            | 
#   -----------------|------------|------------|------------|------------|
#   Column Total |          5 |          7 |          8 |         20 | 
#   |      0.250 |      0.350 |      0.400 |            | 
#   -----------------|------------|------------|------------|------------|

## A. Conclusion
