  pred_fun = function(X) as.numeric(predict(nn, X, batch_size = nrow(X)))
  ks <- kernelshap(
    history,
    dia_small, 
    pred_fun=pred_fun, 
    bg_X = dia_small
  )


library(kernelshap)
library(shapviz)

ind <- sample(nrow(X), 100)
dia_small <- X[ind, ]
pred_fun <- function(mod, X) predict(mod, data.matrix(X), batch_size = 100)
shap_nn <- kernelshap(nn, X, bg_X = dia_small, pred_fun = pred_fun)




system.time (
  explanation <- lime::explain (
    x = as.data.frame(x_train),
    explainer    = explainer, 
    n_features = 6))