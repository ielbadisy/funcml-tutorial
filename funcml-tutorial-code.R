knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.width = 6,
  fig.height = 4,
  fig.align = "center",
  out.width = "85%"
)
library(ggplot2)

# remotes::install_github("ielbadisy/funcml")

library(funcml)

# data(package = "funcml")

data_heart <- heartdisease
data_heart$sex <- factor(data_heart$sex)
data_heart$chest_pain <- factor(data_heart$chest_pain)
data_heart$blood_sugar <- factor(data_heart$blood_sugar,
                                 levels = c(FALSE, TRUE),
                                 labels = c("normal", "high"))
data_heart$exercise_induced_angina <- factor(data_heart$exercise_induced_angina)
data_heart$heart_disease <- factor(data_heart$heart_disease, levels = c("No", "Yes"))

data_reg <- data_heart
formula_reg <- maximum_hr ~ age + sex + chest_pain + bp + cholesterol +
  blood_sugar + exercise_induced_angina + heart_disease
dim(data_reg)

data_cls <- data_heart
formula_cls <- heart_disease ~ age + sex + chest_pain + bp + cholesterol +
  blood_sugar + maximum_hr + exercise_induced_angina
dim(data_cls)
table(data_cls$heart_disease)

data_mc <- data_heart
formula_mc <- chest_pain ~ age + sex + bp + cholesterol + blood_sugar +
  maximum_hr + exercise_induced_angina + heart_disease
dim(data_mc)
table(data_mc$chest_pain)

knitr::include_graphics("funcml_workflow.png")

list_learners() |> head(8)

list_metrics() |> head(8)

list_interpretability_methods()

fold_df <- do.call(rbind, lapply(1:5, function(k) {
  data.frame(fold  = paste0("Iteration ", k),
             block = paste0("Fold ", 1:5),
             set   = ifelse(1:5 == k, "Validation", "Training"))
}))
fold_df$fold  <- factor(fold_df$fold,  levels = rev(paste0("Iteration ", 1:5)))
fold_df$block <- factor(fold_df$block, levels = paste0("Fold ", 1:5))

ggplot(fold_df, aes(block, fold, fill = set)) +
  geom_tile(colour = "white", linewidth = 0.8, height = 0.8) +
  geom_text(aes(label = ifelse(set == "Validation", "test", "train")),
            colour = "white", fontface = "bold", size = 3.4) +
  scale_fill_manual(values = c(Training = "#2166AC", Validation = "#D6604D")) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), legend.position = "bottom")

resamp_df <- dplyr::bind_rows(
  data.frame(row = "Holdout 80/20", obs = 1:20,
             set = ifelse(1:20 %in% 17:20, "Assessment", "Analysis")),
  do.call(rbind, lapply(1:5, function(k)
    data.frame(row = paste0("Fold ", k), obs = 1:20,
               set = ifelse(1:20 %in% ((k-1)*4+1):(k*4), "Assessment", "Analysis"))))
)
resamp_df$row <- factor(resamp_df$row,
  levels = rev(c("Holdout 80/20", paste0("Fold ", 1:5))))

ggplot(resamp_df, aes(obs, row, fill = set)) +
  geom_tile(colour = "white", linewidth = 0.35, height = 0.78) +
  scale_fill_manual(values = c(Analysis = "#2E86C1", Assessment = "#E67E22")) +
  scale_x_continuous(breaks = c(1,5,10,15,20), expand = c(0.01,0.01)) +
  labs(x = "Patient index", y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), legend.position = "bottom")

dot <- "
digraph resampling {
  graph [layout=dot, rankdir=LR, bgcolor='transparent']
  node [shape=box, style='rounded,filled', fontname=Helvetica,
        color=white, fontcolor=white, penwidth=1.2]
  edge [color='#555555', arrowsize=0.8, fontname=Helvetica, fontsize=10]
  start    [label='Data structure?',          fillcolor='#1B4F72']
  iid      [label='Independent records',      fillcolor='#2E86C1']
  cluster  [label='Repeated patients/sites',  fillcolor='#2E86C1']
  time     [label='Calendar time matters',    fillcolor='#2E86C1']
  tune     [label='Tuning on same dataset?',  fillcolor='#566573']
  holdout  [label='Holdout (large n)',         fillcolor='#27AE60']
  kfold    [label='K-fold CV (small/mod n)',   fillcolor='#27AE60']
  group    [label='Group/site CV',             fillcolor='#E67E22']
  temporal [label='Temporal split',            fillcolor='#E67E22']
  nested   [label='Nested CV',                 fillcolor='#C0392B']
  start -> iid; start -> cluster; start -> time
  iid -> tune [label='if model selection']
  iid -> holdout [label='large n']
  iid -> kfold   [label='limited n']
  cluster -> group; time -> temporal; tune -> nested
}"
DiagrammeR::grViz(dot)

temporal <- data.frame(panel = "Temporal split", x = 1:12, y = 2,
  set = c(rep("Train: earlier patients",7), rep("Temporal test: future patients",5)))
site <- data.frame(panel = "Site split", x = 1:12, y = 1,
  set = c(rep("Train sites A/B",8), rep("External site C",4)))
split_df <- rbind(temporal, site)

ggplot(split_df, aes(x, panel, fill = set)) +
  geom_tile(colour = "white", linewidth = 0.4, height = 0.65) +
  scale_fill_manual(values = c("Train: earlier patients"="#2E86C1",
    "Temporal test: future patients"="#D6604D",
    "Train sites A/B"="#27AE60", "External site C"="#8E44AD")) +
  scale_x_continuous(breaks = NULL, expand = c(0.01,0.01)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), legend.position = "bottom")

cv5   <- cv(v = 5, seed = 42)              # 5-fold CV
cv5r  <- cv(v = 5, repeats = 3, seed = 42) # repeated CV
ho    <- holdout(prop = 0.8, seed = 42)    # 80/20 holdout
# group_cv(v = 5, group = "patient_id")   # grouped CV
# time_cv(initial = 50, assess = 10)      # time-series CV

fit_glm_reg <- fit(formula_reg, data = data_reg, model = "glm")
fit_glm_reg

fit_glm_cls <- fit(formula_cls, data = data_cls, model = "glm")
predict(fit_glm_cls, newdata = data_cls[1:3, ], type = "prob")

fit_glmnet <- fit(formula_reg, data = data_reg, model = "glmnet",
                  spec = list(alpha = 0.5))
fit_glmnet

fit_pls <- fit(formula_reg, data = data_reg, model = "pls",
               spec = list(ncomp = 3))
fit_pls

fit_rpart <- fit(formula_cls, data = data_cls, model = "rpart",
                 spec = list(cp = 0.01, minsplit = 10))
fit_rpart

fit_ctree <- fit(formula_cls, data = data_cls, model = "ctree",
                 spec = list(mincriterion = 0.95))
fit_ctree

fit_C50 <- fit(formula_cls, data = data_cls, model = "C50",
               spec = list(trials = 1))
fit_C50

fit_ranger <- fit(formula_reg, data = data_reg, model = "ranger",
                  spec = list(num.trees = 300, mtry = 3, seed = 42))
fit_ranger

fit_rf <- fit(formula_cls, data = data_cls, model = "randomForest",
              spec = list(ntree = 300, mtry = 3))
fit_rf

fit_cforest <- fit(formula_cls, data = data_cls, model = "cforest",
                   spec = list(ntree = 100))
fit_cforest

fit_gbm <- fit(formula_reg, data = data_reg, model = "gbm",
               spec = list(n.trees = 200, interaction.depth = 3,
                           shrinkage = 0.05, n.minobsinnode = 5))
fit_gbm

fit_xgb <- fit(formula_reg, data = data_reg, model = "xgboost",
               spec = list(nrounds = 200, max_depth = 4, eta = 0.05,
                           subsample = 0.8))
fit_xgb

fit_lgbm <- fit(formula_reg, data = data_reg, model = "lightgbm",
                spec = list(num_leaves = 31, learning_rate = 0.05,
                            n_estimators = 200))
fit_lgbm

fit_ada <- fit(formula_cls, data = data_cls, model = "adaboost",
               spec = list(iter = 100, nu = 1, type = "discrete"))
fit_ada

fit_svm <- fit(formula_cls, data = data_cls, model = "e1071_svm",
               spec = list(kernel = "radial", cost = 1, gamma = 0.1))
fit_svm

fit_kknn <- fit(formula_reg, data = data_reg, model = "kknn",
                spec = list(k = 7, kernel = "optimal"))
fit_kknn

formula_gam <- maximum_hr ~ age + bp
fit_gam <- fit(formula_gam, data = data_reg, model = "gam")
fit_gam

fit_earth <- fit(formula_reg, data = data_reg, model = "earth",
                 spec = list(degree = 2, nprune = 10))
fit_earth

fit_bart <- fit(formula_reg, data = data_reg, model = "bart",
                spec = list(ntree = 50, ndpost = 200, nskip = 50,
                            seed = 42))
fit_bart

fit_nb <- fit(formula_cls, data = data_cls, model = "naivebayes")
fit_nb

fit_lda <- fit(formula_cls, data = data_cls, model = "lda")
fit_lda

fit_qda <- fit(formula_cls, data = data_cls, model = "qda")
fit_qda

fit_fda <- fit(formula_cls, data = data_cls, model = "fda")
fit_fda

fit_nnet <- fit(formula_reg, data = data_reg, model = "nnet",
                spec = list(size = 5, decay = 0.01, linout = TRUE,
                            trace = FALSE))
fit_nnet

fit_stack <- fit(formula_reg, data = data_reg, model = "stacking",
                 spec = list(learners = c("glm", "rpart", "ranger"),
                             meta_learner = "glm"))
fit_stack

fit_sl <- fit(formula_reg, data = data_reg, model = "superlearner",
              spec = list(learners = c("glm", "rpart", "ranger")))
fit_sl

truth <- data_reg$maximum_hr
pred  <- predict(fit_ranger, newdata = data_reg)

rmse(truth, pred)
mae(truth, pred)
rsq(truth, pred)

eval_reg <- evaluate(
  data       = data_reg,
  formula    = formula_reg,
  model      = "ranger",
  resampling = cv(v = 5, seed = 42),
  metrics    = c("rmse", "mae", "rsq"),
  seed       = 42
)
eval_reg

eval_cls <- evaluate(
  data       = data_cls,
  formula    = formula_cls,
  model      = "ranger",
  resampling = cv(v = 5, seed = 42),
  metrics    = c("accuracy", "auc", "logloss"),
  type       = "prob",
  seed       = 42
)
eval_cls

plot(eval_reg)

grid_rpart <- expand.grid(
  cp       = c(0.001, 0.01, 0.05),
  minsplit = c(5, 10, 20)
)

tune_rpart <- tune(
  data       = data_reg,
  formula    = formula_reg,
  model      = "rpart",
  grid       = grid_rpart,
  resampling = cv(v = 5, seed = 42),
  metric     = "rmse",
  search     = "grid",
  seed       = 42
)

tune_rpart
tune_rpart$best

grid_ranger <- expand.grid(
  num.trees = c(100, 200, 300),
  mtry      = c(2, 3, 5)
)

tune_ranger <- tune(
  data       = data_reg,
  formula    = formula_reg,
  model      = "ranger",
  grid       = grid_ranger,
  resampling = cv(v = 5, seed = 42),
  metric     = "rmse",
  search     = "random",
  n_evals    = 6,
  seed       = 42
)

tune_ranger$best

plot(tune_rpart)

outer <- data.frame(x = 1:5, y = 3,
  label = paste0("Outer\n", 1:5),
  set   = c("Test","Train","Train","Train","Train"))
inner <- data.frame(x = 2:5, y = 1.4,
  label = c("Inner\nvalid","Inner\ntrain","Inner\ntrain","Inner\ntrain"),
  set   = c("Validation","Train","Train","Train"))
cv_df       <- rbind(outer, inner)

ggplot(cv_df, aes(x, y, fill = set)) +
  geom_tile(width = 0.85, height = 0.8, colour = "white", linewidth = 0.8) +
  geom_text(aes(label = label), colour = "white", fontface = "bold",
            size = 3.2, lineheight = 0.85) +
  annotate("text", x = 3,   y = 3.7, fontface = "bold", size = 3.5,
           label = "Outer loop: one test fold is untouched") +
  annotate("text", x = 3.5, y = 2.05, fontface = "bold", size = 3.5,
           label = "Inner loop: tune using only outer-training data") +
  annotate("segment", x = 2, xend = 2, y = 2.55, yend = 1.85,
           arrow = grid::arrow(length = grid::unit(0.15,"cm")),
           colour = "#555555") +
  scale_fill_manual(values = c(Train="#2E86C1", Validation="#E67E22",
                               Test="#C0392B")) +
  coord_cartesian(xlim = c(0.4,5.6), ylim = c(0.7,4.0)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_void(base_size = 11) +
  theme(legend.position = "bottom")

nested <- tune(
  data             = data_reg,
  formula          = formula_reg,
  model            = "rpart",
  grid             = grid_rpart,
  resampling       = cv(v = 3, seed = 1),
  outer_resampling = cv(v = 4, seed = 2),
  metric           = "rmse",
  seed             = 42
)
nested

cmp <- compare_learners(
  data       = data_reg,
  formula    = formula_reg,
  models     = c("glm", "rpart", "ranger", "gbm", "xgboost"),
  resampling = cv(v = 5, seed = 42),
  metrics    = c("rmse", "mae"),
  seed       = 42
)
cmp

plot(cmp)

fit_rf_reg <- fit(formula_reg, data = data_reg, model = "ranger",
                  spec = list(num.trees = 300, seed = 42))

vip_obj <- interpret(fit_rf_reg, data_reg, method = "vip")
plot(vip_obj)

perm_obj <- interpret(fit_rf_reg, data_reg, method = "permute",
                      metric = "rmse", nsim = 20, seed = 42)
plot(perm_obj)

pdp_obj <- interpret(fit_rf_reg, data_reg, method = "pdp",
                     features = "bp")
plot(pdp_obj)

ice_obj <- interpret(fit_rf_reg, data_reg, method = "ice",
                     features = "bp")
plot(ice_obj)

ale_obj <- interpret(fit_rf_reg, data_reg, method = "ale",
                     features = "bp")
plot(ale_obj)

lime_obj <- interpret(fit_rf_reg, data_reg, method = "lime",
                      newdata = data_reg[5, , drop = FALSE],
                      features = c("age", "bp", "cholesterol"),
                      k = 3, seed = 42)
plot(lime_obj)

local_obj <- interpret(fit_rf_reg, data_reg, method = "local_model",
                       newdata = data_reg[5, , drop = FALSE],
                       features = c("age", "bp", "cholesterol"),
                       k = 10, seed = 42)
plot(local_obj)

shap_local <- interpret(fit_rf_reg, data_reg, method = "shap",
                        newdata = data_reg[5, , drop = FALSE],
                        nsim = 50, seed = 42)
plot(shap_local, kind = "waterfall")

shap_all <- interpret(fit_rf_reg, data_reg, method = "shap",
                      newdata = data_reg,
                      nsim = 50, seed = 42)
plot(shap_all, kind = "beeswarm")

plot(shap_all, kind = "bar")

surr_obj <- interpret(fit_rf_reg, data_reg, method = "surrogate",
                      spec = list(cp = 0.01))
plot(surr_obj)

# int_obj <- interpret(fit_rf_reg, data_reg, method = "interaction")
# plot(int_obj)

cp_obj <- interpret(fit_rf_reg, data_reg,
                    method  = "ceteris_paribus",
                    newdata = data_reg[5, , drop = FALSE],
                    features = c("bp", "age", "cholesterol"))
plot(cp_obj)

fit_rf_cls <- fit(formula_cls, data = data_cls, model = "ranger",
                  spec = list(num.trees = 300, seed = 42))

cal_obj <- interpret(fit_rf_cls, data_cls, method = "calibration",
                     type = "prob", pos_level = "Yes", bins = 10)
plot(cal_obj, style = "curve")

data_causal <- data_reg

ate_obj <- estimate(
  data      = data_causal,
  formula   = maximum_hr ~ exercise_induced_angina + age + sex + chest_pain +
    bp + cholesterol + blood_sugar + heart_disease,
  model     = "ranger",
  treatment = "exercise_induced_angina",
  estimand  = "ATE",
  interval  = "bootstrap",
  n_boot    = 200,
  seed      = 42
)
ate_obj

plot(ate_obj)

att_obj <- estimate(
  data      = data_causal,
  formula   = maximum_hr ~ exercise_induced_angina + age + sex + chest_pain +
    bp + cholesterol + blood_sugar + heart_disease,
  model     = "ranger",
  treatment = "exercise_induced_angina",
  estimand  = "ATT",
  interval  = "bootstrap",
  n_boot    = 200,
  seed      = 42
)
att_obj

cate_obj <- estimate(
  data      = data_causal,
  formula   = maximum_hr ~ exercise_induced_angina + age + sex + chest_pain +
    bp + cholesterol + blood_sugar + heart_disease,
  model     = "ranger",
  treatment = "exercise_induced_angina",
  estimand  = "CATE",
  newdata   = data_causal,
  seed      = 42
)
head(cate_obj$estimates)

cmp_cls <- compare_learners(
  data       = data_cls,
  formula    = formula_cls,
  models     = c("glm", "rpart", "ranger", "gbm", "xgboost"),
  resampling = cv(v = 5, seed = 42),
  metrics    = c("accuracy", "auc", "logloss"),
  type       = "prob",
  seed       = 42
)
cmp_cls

plot(cmp_cls)

grid_xgb <- expand.grid(
  nrounds   = c(100, 200),
  max_depth = c(3, 4, 6),
  eta       = c(0.05, 0.1)
)

tuned_xgb <- tune(
  data       = data_cls,
  formula    = formula_cls,
  model      = "xgboost",
  grid       = grid_xgb,
  resampling = cv(v = 5, seed = 42),
  metric     = "auc",
  search     = "random",
  n_evals    = 8,
  type       = "prob",
  seed       = 42
)

tuned_xgb$best

fit_final <- fit(
  formula = formula_cls,
  data    = data_cls,
  model   = "xgboost",
  spec    = as.list(tuned_xgb$best[1, ])
)
fit_final

perm_cls <- interpret(fit_final, data_cls,
                      method = "permute",
                      metric = "auc",
                      type   = "prob",
                      nsim   = 20,
                      seed   = 42)
plot(perm_cls)

pdp_hr <- interpret(fit_final, data_cls,
                     method   = "pdp",
                     features = "maximum_hr",
                     type     = "prob")
plot(pdp_hr)

ale_hr <- interpret(fit_final, data_cls,
                     method   = "ale",
                     features = "maximum_hr",
                     type     = "prob")
plot(ale_hr)

high_risk <- data_cls[which.max(
  predict(fit_final, data_cls, type = "prob")[, "Yes"]
), , drop = FALSE]

shap_hr <- interpret(fit_final, data_cls,
                     method  = "shap",
                     newdata = high_risk,
                     type    = "prob",
                     nsim    = 50, seed = 42)
plot(shap_hr, kind = "waterfall")

cal_final <- interpret(fit_final, data_cls,
                       method    = "calibration",
                       type      = "prob",
                       pos_level = "Yes",
                       bins      = 10)
plot(cal_final, style = "curve")

sessionInfo()
