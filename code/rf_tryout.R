install.packages("ISLR2")
install.packages("tree")

library(ISLR2)
library(tree)

help(ISLR)

# 8.3.2 fitting regression trees
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv ~ ., Boston, subset = train)

summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty = 0)

cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")

prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

yhat <-predict(tree.boston, newdata = Boston[-train, ])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0, 1)

mean((yhat -boston.test)^2)

# random forests and bagging
# bagging
install.packages("randomForest")
library(randomForest)
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 12, importance = TRUE)
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0, 1)
mean((yhat.bag -boston.test)^2)

# random forest
rf.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 6, importance = TRUE)
yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])
plot(yhat.rf, boston.test)
abline(0, 1)
mean((yhat.rf -boston.test)^2)

importance(rf.boston)
varImpPlot(rf.boston)
