# ==============================================================================
# 02. MODELOS, UMBRAL PRESPECIFICADO Y AUDITORÍA DE EQUIDAD (CON IC 95%)
# Requiere haber ejecutado 01_cohorte_y_diagnosticos.R (genera los .rds).
# Responde a los comentarios 7 (prevalencia en test), 11, 13, 14, 16 y 17.
# [KE] Revisado por Katrina Espinar Herranz (14/10/2026): cambios marcados con [KE].
# ==============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(survey); library(rsample); library(recipes)
  library(parsnip); library(workflows); library(tune); library(hardhat); library(xgboost); library(glmnet)
})
options(survey.lonely.psu = "adjust")
data_dir <- Sys.getenv("NHANES_DIR", "~/Desktop/CONGRESO_IA_STANFORD")
cohort <- readRDS(file.path(data_dir, "cohorte_analitica.rds"))
marco  <- readRDS(file.path(data_dir, "marco_mec.rds"))

TARGET_SENSITIVITY <- 0.90   # prespecificada (comentario 13). No cambiar tras ver resultados.
N_REPLICATES <- 1000

# ---------- PREDICTORES ----------
yn <- function(x, borderline = FALSE) {
  lv <- if (borderline) c("Si", "No", "Borderline") else c("Si", "No")
  factor(case_when(x == 1 ~ "Si", x == 2 ~ "No", borderline & x == 3 ~ "Borderline", TRUE ~ NA_character_), levels = lv)
}
nan2na <- function(x) ifelse(is.nan(x), NA_real_, x)
dat <- cohort %>%
  mutate(
    SBP_mean = nan2na(rowMeans(cbind(BPXOSY1, BPXOSY2, BPXOSY3), na.rm = TRUE)),
    DBP_mean = nan2na(rowMeans(cbind(BPXODI1, BPXODI2, BPXODI3), na.rm = TRUE)),
    sexo     = factor(ifelse(RIAGENDR == 2, "Mujer", "Hombre")),
    DIQ010_f = yn(DIQ010, borderline = TRUE),
    BPQ020_f = yn(BPQ020), SMQ020_f = yn(SMQ020),
    OCD150_f = factor(case_when(OCD150 == 1 ~ "Trabajando", OCD150 == 2 ~ "Con_trabajo_ausente",
                                OCD150 == 3 ~ "Buscando", OCD150 == 4 ~ "No_trabaja", TRUE ~ NA_character_)),
    case_wts = importance_weights(WTMECPRP / mean(WTMECPRP))
  )

# ---------- PARTICIÓN 70/30 ----------
set.seed(2026)
spl   <- initial_split(dat, prop = 0.70, strata = Target_2021)
train <- training(spl); test <- testing(spl)

pred_s1 <- c("RIDAGEYR", "sexo", "BMXBMI", "BMXWAIST", "SBP_mean", "DBP_mean",
             "DIQ010_f", "BPQ020_f", "SMQ020_f", "OCD150_f", "INDFMPIR")
# [KE] Quitado LBXSBU (nitrógeno ureico, BUN): es un marcador de función renal que se pide en el
# mismo panel que la creatinina -> fuga indirecta del desenlace. Se añade al control de fuga.
pred_s2 <- c(pred_s1, "LBXSGL", "LBXSUA", "LBXTC")   # sin creatinina, UACR ni BUN
stopifnot(!any(c("LBXSCR", "URDACT", "UACR", "LBXSBU", "RIDRETH3", "es_afro") %in% pred_s2))  # sin fuga ni raza

# Imputación DENTRO de la receta: se aprende solo con los datos de entrenamiento
# (y dentro de cada fold en la validación cruzada). Mediana (numéricas) y moda (categóricas).
# [KE] Con case weights en el workflow, recipes puede usar los pesos en los pasos que los admiten;
# no lo describáis como "no ponderada" en el abstract.
make_rec <- function(preds) {
  recipe(train[, c("Target_2021", preds, "case_wts")]) %>%
    update_role(all_of(preds), new_role = "predictor") %>%
    update_role(Target_2021, new_role = "outcome") %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors())
}
lr_spec  <- logistic_reg(penalty = 0.01, mixture = 0.5) %>% set_engine("glmnet") %>% set_mode("classification")
xgb_spec <- boost_tree(trees = 500, tree_depth = 4, learn_rate = 0.05) %>%
  set_engine("xgboost", nthread = 1) %>% set_mode("classification")
wf <- function(preds, spec) workflow() %>% add_recipe(make_rec(preds)) %>% add_model(spec) %>% add_case_weights(case_wts)

wf_xgb1 <- wf(pred_s1, xgb_spec)   # MODELO PRINCIPAL (Etapa 1, Zero-Lab)
wf_xgb2 <- wf(pred_s2, xgb_spec)   # análisis secundario (Etapa 2, Lab-Enhanced)
wf_lr   <- wf(pred_s1, lr_spec)    # referencia: regresión logística con predictores de la ETAPA 1

set.seed(2026); fit_xgb1 <- fit(wf_xgb1, train)
set.seed(2026); fit_xgb2 <- fit(wf_xgb2, train)
set.seed(2026); fit_lr   <- fit(wf_lr,   train)

# ---------- UMBRAL: sensibilidad ponderada >= 90% en predicciones OOF del entrenamiento ----------
set.seed(2026)
folds <- vfold_cv(train, v = 5, strata = Target_2021)
oof <- fit_resamples(wf_xgb1, folds, control = control_resamples(save_pred = TRUE)) %>%
  collect_predictions() %>% arrange(.row)
stopifnot(nrow(oof) == nrow(train), all(oof$.row == seq_len(nrow(train))))
w_oof <- train$WTMECPRP[oof$.row]; y_oof <- oof$Target_2021 == "Case"
cand <- sort(unique(oof$.pred_Case))
sens <- vapply(cand, function(t) sum(w_oof * (y_oof & oof$.pred_Case >= t)) / sum(w_oof * y_oof), 0)
spec <- vapply(cand, function(t) sum(w_oof * (!y_oof & oof$.pred_Case < t)) / sum(w_oof * !y_oof), 0)
ok <- which(sens >= TARGET_SENSITIVITY)
stopifnot(length(ok) > 0)
thr <- max(cand[ok])   # la sensibilidad es monótona decreciente en t: el mayor t factible maximiza la especificidad
cat(sprintf("\nUmbral prespecificado (sens. OOF ponderada >= %.0f%%): %.4f | sens. OOF = %.3f | espec. OOF = %.3f\n",
            100 * TARGET_SENSITIVITY, thr, sens[cand == thr], spec[cand == thr]))

# ---------- PREDICCIONES EN TEST ----------
pt <- test %>% transmute(SEQN,
  p_xgb1 = predict(fit_xgb1, test, type = "prob")$.pred_Case,
  p_xgb2 = predict(fit_xgb2, test, type = "prob")$.pred_Case,
  p_lr   = predict(fit_lr,   test, type = "prob")$.pred_Case)
pt$pred_case <- as.numeric(pt$p_xgb1 >= thr)

marco <- marco %>% left_join(pt, by = "SEQN") %>%
  mutate(in_test = !is.na(p_xgb1),
         across(c(p_xgb1, p_xgb2, p_lr, pred_case), ~ coalesce(.x, 0)))
stopifnot(sum(marco$in_test) == nrow(test))

# ---------- PREVALENCIA EN EL TEST (comentario 7) ----------
des <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMECPRP, nest = TRUE, data = marco)
prev_line <- function(d, lab) {
  dd <- d$variables[d$prob < Inf, ]
  f <- function(v) { e <- svyciprop(as.formula(paste0("~", v)), d, method = "logit"); ci <- attr(e, "ci")
                     sprintf("%.1f%% (%.1f–%.1f)", 100 * coef(e), 100 * ci[1], 100 * ci[2]) }
  data.frame(grupo = lab, n = nrow(dd),
             casos_2021 = sum(dd$case21), sin_pond_2021 = sprintf("%.1f%%", 100 * mean(dd$case21)), pond_2021 = f("case21"),
             casos_2009 = sum(dd$case09), sin_pond_2009 = sprintf("%.1f%%", 100 * mean(dd$case09)), pond_2009 = f("case09"))
}
cat("\n===== PREVALENCIA EN EL CONJUNTO DE TEST =====\n")
print(rbind(prev_line(subset(des, in_test), "Test completo"),
            prev_line(subset(des, in_test & subgrupo), "Test: Black no hispano PIR<1.35"),
            prev_line(subset(des, in_test & resto), "Test: resto")), row.names = FALSE)

# ---------- N SIN PONDERAR QUE SUSTENTAN CADA MÉTRICA (comentario 17) ----------
tt <- marco[marco$in_test, ]
celdas <- function(idx, cs) c(n = sum(idx), casos = sum(idx & cs == 1), no_casos = sum(idx & cs == 0),
                              FN = sum(idx & cs == 1 & tt$pred_case == 0), TN = sum(idx & cs == 0 & tt$pred_case == 0))
cat("\n===== CELDAS SIN PONDERAR EN TEST =====\n")
print(rbind(`Subgrupo 2021` = celdas(tt$subgrupo, tt$case21), `Resto 2021` = celdas(tt$resto, tt$case21),
            `Subgrupo 2009` = celdas(tt$subgrupo, tt$case09), `Resto 2009` = celdas(tt$resto, tt$case09)))

# ---------- MÉTRICAS PONDERADAS + BOOTSTRAP DE DISEÑO ----------
w_auc <- function(y, s, w) {           # AUC ponderada (Mann-Whitney ponderado, empates = 1/2)
  o <- order(s); s <- s[o]; y <- y[o]; w <- w[o]
  wn <- ifelse(y == 0, w, 0); wc <- ifelse(y == 1, w, 0)
  g <- split(seq_along(s), s)          # agrupar empates
  cum_neg <- 0; num <- 0
  for (ix in g) { tn <- sum(wn[ix]); tc <- sum(wc[ix]); num <- num + tc * (cum_neg + 0.5 * tn); cum_neg <- cum_neg + tn }
  num / (sum(wc) * sum(wn))
}
metricas <- function(w, data) {
  t <- data$in_test; pc <- data$pred_case
  rate <- function(num, den) sum(w * num) / sum(w * den)
  fnr  <- function(g, cs) rate(t & g & cs == 1 & pc == 0, t & g & cs == 1)
  spc  <- function(g, cs) rate(t & g & cs == 0 & pc == 0, t & g & cs == 0)
  S <- data$subgrupo; R <- data$resto; A <- rep(TRUE, length(w))
  x <- c(auc_xgb1 = w_auc(data$case21[t], data$p_xgb1[t], w[t]),
         auc_xgb2 = w_auc(data$case21[t], data$p_xgb2[t], w[t]),
         auc_lr   = w_auc(data$case21[t], data$p_lr[t],   w[t]),
         sens_global_2021 = 1 - fnr(A, data$case21), spec_global_2021 = spc(A, data$case21),
         fnr_sub_2021 = fnr(S, data$case21), fnr_resto_2021 = fnr(R, data$case21),
         fnr_sub_2009 = fnr(S, data$case09), fnr_resto_2009 = fnr(R, data$case09),
         spec_sub_2021 = spc(S, data$case21), spec_resto_2021 = spc(R, data$case21),
         spec_sub_2009 = spc(S, data$case09), spec_resto_2009 = spc(R, data$case09))
  c(x, dif_fnr_2021 = unname(x["fnr_sub_2021"] - x["fnr_resto_2021"]),
       dif_fnr_2009 = unname(x["fnr_sub_2009"] - x["fnr_resto_2009"]),
       dif_spec_2021 = unname(x["spec_sub_2021"] - x["spec_resto_2021"]),
       dif_spec_2009 = unname(x["spec_sub_2009"] - x["spec_resto_2009"]),
       dif_auc_xgb1_lr = unname(x["auc_xgb1"] - x["auc_lr"]),
       dif_auc_xgb2_xgb1 = unname(x["auc_xgb2"] - x["auc_xgb1"]))
}
set.seed(2026)
rep_des <- as.svrepdesign(des, type = "subbootstrap", replicates = N_REPLICATES)
res <- withReplicates(rep_des, metricas, return.replicates = TRUE)
est <- coef(res); se <- sqrt(diag(as.matrix(vcov(res))))
df_d <- degf(des); crit <- qt(0.975, df_d)
stopifnot(!anyNA(res$replicates))
tabla <- data.frame(metrica = names(est), estimacion = round(est, 4),
                    IC95_inf = round(est - crit * se, 4), IC95_sup = round(est + crit * se, 4),
                    IC95_percentil_inf = round(apply(res$replicates, 2, quantile, 0.025), 4),
                    IC95_percentil_sup = round(apply(res$replicates, 2, quantile, 0.975), 4))
cat(sprintf("\n===== RESULTADOS EN TEST (bootstrap de diseño, %d réplicas, gl = %d) =====\n", N_REPLICATES, df_d))
print(tabla, row.names = FALSE)
write.csv(tabla, file.path(data_dir, "resultados_test.csv"), row.names = FALSE)
cat("\nNota: IC = estimación ± t(gl) × EE bootstrap. Incertidumbre condicional al modelo y al umbral ya fijados.\n")
# [KE] sessionInfo() movido al final del script.
# [KE] Para el abstract necesitáis de esta tabla: auc_xgb2 (nuevo, sin BUN), dif_auc_xgb1_lr,
#      dif_fnr_2021, dif_fnr_2009 y dif_spec_2021 (si el IC incluye 0, no hay diferencia demostrable).

# ---------- IC KORN-GRAUBARD PARA FNR Y ESPECIFICIDAD (recomendado por NCHS; válido con 0 eventos) ----------
# Con 0 falsos negativos el EE bootstrap es 0 y el IC bootstrap es degenerado; este IC no lo es.
kg <- function(cond_den, cond_num, lab) {
  d <- subset(des, in_test & eval(cond_den, des$variables))
  d$variables$evento <- as.numeric(eval(cond_num, d$variables))
  n_obs <- sum(d$prob < Inf); n_ev <- sum(d$variables$evento[d$prob < Inf])
  e <- svyciprop(~evento, d, method = "beta", df = degf(des)); ci <- attr(e, "ci")
  # Con 0 eventos la varianza de diseño es 0 y KG no está definido: se usa Clopper-Pearson
  # exacto con el n observado (límite superior = 1 - 0.025^(1/n)), como hace NCHS.
  # [KE] Correcto. Al usar el n observado (no el tamaño muestral efectivo) el IC es algo optimista;
  #      basta con decir en el abstract "Clopper–Pearson when no events occurred".
  if (n_ev == 0) ci <- c(0, qbeta(0.975, 1, n_obs))
  if (n_ev == n_obs) ci <- c(qbeta(0.025, n_obs, 1), 1)
  data.frame(metrica = lab, n = n_obs, eventos = n_ev,
             estimacion = round(coef(e), 4), KG_inf = round(ci[1], 4), KG_sup = round(ci[2], 4))
}
kgt <- rbind(
  kg(quote(subgrupo & case21 == 1), quote(pred_case == 0), "FNR subgrupo 2021"),
  kg(quote(resto    & case21 == 1), quote(pred_case == 0), "FNR resto 2021"),
  kg(quote(subgrupo & case09 == 1), quote(pred_case == 0), "FNR subgrupo 2009"),
  kg(quote(resto    & case09 == 1), quote(pred_case == 0), "FNR resto 2009"),
  kg(quote(subgrupo & case21 == 0), quote(pred_case == 0), "Especificidad subgrupo 2021"),
  kg(quote(resto    & case21 == 0), quote(pred_case == 0), "Especificidad resto 2021"),
  kg(quote(subgrupo & case09 == 0), quote(pred_case == 0), "Especificidad subgrupo 2009"),
  kg(quote(resto    & case09 == 0), quote(pred_case == 0), "Especificidad resto 2009"),
  kg(quote(case21 == 1), quote(pred_case == 1), "Sensibilidad global 2021"),
  kg(quote(case21 == 0), quote(pred_case == 0), "Especificidad global 2021"))
cat("\n===== FNR Y ESPECIFICIDAD CON IC 95% KORN-GRAUBARD =====\n"); print(kgt, row.names = FALSE)
cat(sprintf("\nPorcentaje ponderado del test marcado como positivo: %.1f%%\n",
            100 * coef(svymean(~pred_case, subset(des, in_test)))))
cat(sprintf("Porcentaje ponderado del subgrupo marcado como positivo: %.1f%%\n",
            100 * coef(svymean(~pred_case, subset(des, in_test & subgrupo)))))
write.csv(kgt, file.path(data_dir, "resultados_korn_graubard.csv"), row.names = FALSE)

# [KE] Información de la sesión (versiones de paquetes), al final para que no corte la salida de resultados
sessionInfo()
