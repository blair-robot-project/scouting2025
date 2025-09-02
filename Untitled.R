library(tidyverse)
library(dslabs)
library(caret)
library(purrr)

data("mnist_27")
mnist_27$test |> ggplot(aes(x_1, x_2, color = y)) +  geom_point()
knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 5)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
#confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]
#> Accuracy 
#>    0.815

fit_lm <- mnist_27$train |> 
    mutate(y = ifelse(y == 7, 1, 0)) |> 
    lm(y ~ x_1 + x_2, data = _)
p_hat_lm <- predict(fit_lm, mnist_27$test)
y_hat_lm <- factor(ifelse(p_hat_lm > 0.5, 7, 2))
#confusionMatrix(y_hat_lm, mnist_27$test$y)$overall["Accuracy"]

#> Accuracy 
#>    0.775

ks <- seq(3, 251, 2)
accuracy <- map_df(ks, function(k){
    fit <- knn3(y ~ ., data = mnist_27$train, k = k)
    
    y_hat <- predict(fit, mnist_27$train, type = "class")
    cm_train <- confusionMatrix(y_hat, mnist_27$train$y)
    train_error <- cm_train$overall["Accuracy"]
    
    y_hat <- predict(fit, mnist_27$test, type = "class")
    cm_test <- confusionMatrix(y_hat, mnist_27$test$y)
    test_error <- cm_test$overall["Accuracy"]
    
    tibble(k = k, train = train_error, test = test_error)
 })

accuracy <- accuracy |>
    pivot_longer(
        cols = c(train, test),
        names_to = "labels",
        values_to = "accuracy"
    )

ggplot(accuracy, aes(x = k, y = accuracy, color = labels)) +
    geom_line(size = 1) +
    geom_point(size = 1.5) +
    labs(title = "Train vs. Test Accuracy as K increases", x = "ks", y = "Accuracy Rate")+
    theme_bw()

