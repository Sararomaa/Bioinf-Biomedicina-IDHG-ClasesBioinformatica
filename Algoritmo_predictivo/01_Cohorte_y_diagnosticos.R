# ==============================================================================
# 01. COHORTE, FLUJO DE EXCLUSIONES, PREVALENCIAS Y RECLASIFICACIÓN
# NHANES 2017–March 2020 (pre-pandemia). Responde a los comentarios 5, 7 y 15
# de la profesora. No depende de ningún modelo.
# [KE] Revisado por Katrina Espinar Herranz (14/10/2026): cambios marcados con [KE].
# ==============================================================================
suppressPackageStartupMessages({ library(haven); library(dplyr); library(purrr); library(survey) })
options(survey.lonely.psu = "adjust")

data_dir <- Sys.getenv("NHANES_DIR", "~/Desktop/CONGRESO_IA_STANFORD")
rd <- function(f) {
  p <- file.path(data_dir, paste0(f, ".xpt"))
  if (!file.exists(p)) stop("No se encuentra ", p)
  read_xpt(p)
}

demo <- rd("P_DEMO")   %>% select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3, INDFMPIR, WTMECPRP, SDMVPSU, SDMVSTRA)
bio  <- rd("P_BIOPRO") %>% select(SEQN, LBXSCR, LBXSGL, LBXSUA, LBXSBU)
alb  <- rd("P_ALB_CR") %>% select(SEQN, URDACT)
tch  <- rd("P_TCHOL")  %>% select(SEQN, LBXTC)
bmx  <- rd("P_BMX")    %>% select(SEQN, BMXBMI, BMXWAIST)
bpx  <- rd("P_BPXO")   %>% select(SEQN, BPXOSY1, BPXOSY2, BPXOSY3, BPXODI1, BPXODI2, BPXODI3)
kiq  <- rd("P_KIQ_U")  %>% select(SEQN, KIQ022)
ocq  <- rd("P_OCQ")    %>% select(SEQN, OCD150)
smq  <- rd("P_SMQ")    %>% select(SEQN, SMQ020)
diq  <- rd("P_DIQ")    %>% select(SEQN, DIQ010)
bpq  <- rd("P_BPQ")    %>% select(SEQN, BPQ020)
comps <- list(demo, bio, alb, tch, bmx, bpx, kiq, ocq, smq, diq, bpq)
stopifnot(all(map_lgl(comps, ~ !anyDuplicated(.x$SEQN))))
nh <- reduce(comps, left_join, by = "SEQN")
stopifnot(nrow(nh) == nrow(demo))

egfr21 <- function(scr, age, sex) {
  k <- ifelse(sex == 2, 0.7, 0.9); a <- ifelse(sex == 2, -0.241, -0.302)
  142 * pmin(scr / k, 1)^a * pmax(scr / k, 1)^(-1.200) * 0.9938^age * ifelse(sex == 2, 1.012, 1)
}
egfr09 <- function(scr, age, sex, black) {
  k <- ifelse(sex == 2, 0.7, 0.9); a <- ifelse(sex == 2, -0.329, -0.411)
  141 * pmin(scr / k, 1)^a * pmax(scr / k, 1)^(-1.209) * 0.993^age *   # Levey et al. 2009 (Ann Intern Med) publica 0.993
    ifelse(sex == 2, 1.018, 1) * ifelse(black == 1, 1.159, 1)
}
# Comprobación con valores de referencia calculados a mano:
# mujer 50 a, Scr 0.7 -> 2021: 142*0.9938^50*1.012 = 105.3 ; hombre 60 a, Scr 1.8 (2021) = 142*2^-1.2*0.9938^60
stopifnot(abs(egfr21(0.7, 50, 2) - 142 * 0.9938^50 * 1.012) < 1e-8)
stopifnot(abs(egfr21(1.8, 60, 1) - 142 * 2^-1.2 * 0.9938^60) < 1e-8)

# ---------- FLUJO DE EXCLUSIONES ----------
f0 <- nh
f1 <- f0 %>% filter(RIDAGEYR >= 20)
f2 <- f1 %>% filter(!is.na(WTMECPRP), WTMECPRP > 0)                 # examinados en el MEC
f3 <- f2 %>% filter(KIQ022 %in% c(1, 2))                             # excluye 7/9/perdido
f4 <- f3 %>% filter(KIQ022 == 2)                                     # excluye diagnóstico previo
f5 <- f4 %>% filter(!is.na(LBXSCR), LBXSCR > 0)                      # creatinina sérica
f6 <- f5 %>% filter(!is.na(URDACT))                                  # UACR
flujo <- tibble(
  paso = c("Participantes 2017–Mar 2020 (todas las edades)", "Adultos >=20 años",
           "Examinados en MEC (WTMECPRP>0)", "KIQ022 válido (1/2)",
           "Sin diagnóstico previo (KIQ022=2)", "Con creatinina sérica",
           "Con UACR = COHORTE ANALÍTICA"),
  n = c(nrow(f0), nrow(f1), nrow(f2), nrow(f3), nrow(f4), nrow(f5), nrow(f6))
) %>% mutate(excluidos = c(NA, -diff(n)))
cat("\n===== FLUJO DE PARTICIPANTES =====\n"); print(as.data.frame(flujo), row.names = FALSE)
cat("Detalle KIQ022 en examinados >=20: ");  print(table(f2$KIQ022, useNA = "ifany"))

cohort <- f6 %>%
  mutate(
    es_afro  = as.integer(RIDRETH3 == 4),
    UACR     = URDACT,
    eGFR_2021 = egfr21(LBXSCR, RIDAGEYR, RIAGENDR),
    eGFR_2009 = egfr09(LBXSCR, RIDAGEYR, RIAGENDR, es_afro),
    PIR_Bajo = case_when(is.na(INDFMPIR) ~ NA_character_, INDFMPIR < 1.35 ~ "Bajo_Ingreso", TRUE ~ "Alto_Ingreso"),
    Target_2021 = factor(ifelse(eGFR_2021 < 60 | UACR >= 30, "Case", "Control"), levels = c("Control", "Case")),
    Target_2009 = factor(ifelse(eGFR_2009 < 60 | UACR >= 30, "Case", "Control"), levels = c("Control", "Case")),
    # Subgrupo: non-Hispanic Black con PIR < 1.35 observado
    subgrupo = es_afro == 1 & !is.na(PIR_Bajo) & PIR_Bajo == "Bajo_Ingreso",
    # Indeterminado: negro con PIR desconocido (no se puede asignar a ningún grupo)
    indeterminado = es_afro == 1 & is.na(PIR_Bajo),
    resto = !subgrupo & !indeterminado
  )
stopifnot(all(is.finite(cohort$eGFR_2021)), all(is.finite(cohort$eGFR_2009)),
          !anyNA(cohort$Target_2021), !anyNA(cohort$Target_2009))
saveRDS(cohort, file.path(data_dir, "cohorte_analitica.rds"))

# ---------- DISEÑO: marco = todos los examinados MEC; la cohorte es un dominio ----------
marco <- nh %>% filter(!is.na(WTMECPRP), WTMECPRP > 0) %>%
  left_join(cohort %>% select(SEQN, Target_2021, Target_2009, subgrupo, indeterminado, resto, es_afro),
            by = "SEQN") %>%
  mutate(in_cohort = !is.na(Target_2021),
         case21 = as.numeric(coalesce(Target_2021 == "Case", FALSE)),
         case09 = as.numeric(coalesce(Target_2009 == "Case", FALSE)),
         up   = as.numeric(in_cohort & Target_2009 == "Control" & Target_2021 == "Case") %>% coalesce(0),
         down = as.numeric(in_cohort & Target_2009 == "Case" & Target_2021 == "Control") %>% coalesce(0),
         reclas = up + down,
         subgrupo = coalesce(subgrupo, FALSE), resto = coalesce(resto, FALSE))
des <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMECPRP, nest = TRUE, data = marco)
cat("\nGrados de libertad del diseño:", degf(des), "\n")
saveRDS(marco, file.path(data_dir, "marco_mec.rds"))

prev <- function(d, lab) {
  p <- function(v) { e <- svyciprop(as.formula(paste0("~", v)), d, method = "logit")
                     ci <- attr(e, "ci"); sprintf("%.1f%% (%.1f–%.1f)", 100*coef(e), 100*ci[1], 100*ci[2]) }
  dd <- d$variables[d$prob < Inf, ]
  tibble(grupo = lab, n = nrow(dd),
         casos_2021 = sum(dd$case21), pct_2021_sin_ponderar = sprintf("%.1f%%", 100*mean(dd$case21)),
         prev_2021_ponderada = p("case21"),
         casos_2009 = sum(dd$case09), pct_2009_sin_ponderar = sprintf("%.1f%%", 100*mean(dd$case09)),
         prev_2009_ponderada = p("case09"))
}
dc <- subset(des, in_cohort)
tab_prev <- bind_rows(prev(dc, "Cohorte completa"),
                      prev(subset(des, in_cohort & subgrupo), "Black no hispano PIR<1.35"),
                      prev(subset(des, in_cohort & resto), "Resto de participantes"))
cat("\n===== PREVALENCIA DE ERC NO DIAGNOSTICADA (IC 95%) =====\n"); print(as.data.frame(tab_prev), row.names = FALSE)

# ---------- RECLASIFICACIÓN ----------
reclas <- function(d, lab) {
  dd <- d$variables[d$prob < Inf, ]
  t <- svytotal(~up + down, d); m <- svymean(~up + down + reclas, d)
  tibble(grupo = lab, n = nrow(dd),
         control09_a_caso21_n = sum(dd$up), caso09_a_control21_n = sum(dd$down),
         reclasificados_n = sum(dd$reclas),
         # [KE] Añadido: % SIN ponderar, para no mezclar n sin ponderar con % ponderado en el abstract
         reclasificados_pct_sin_ponderar = sprintf("%.2f%%", 100*mean(dd$reclas)),
         reclasificados_pct_ponderado = sprintf("%.2f%%", 100*coef(m)["reclas"]),
         poblacion_EEUU_control_a_caso = round(coef(t)["up"]),
         poblacion_EEUU_caso_a_control = round(coef(t)["down"]))
}
tab_rec <- bind_rows(reclas(dc, "Cohorte completa"),
                     reclas(subset(des, in_cohort & es_afro == 1), "Black no hispano (todos)"),
                     reclas(subset(des, in_cohort & subgrupo), "Black no hispano PIR<1.35"),
                     reclas(subset(des, in_cohort & resto), "Resto de participantes"))
cat("\n===== RECLASIFICACIÓN CKD-EPI 2009 -> 2021 =====\n"); print(as.data.frame(tab_rec), row.names = FALSE)
cat("\nTabla cruzada sin ponderar (cohorte):\n"); print(table(CKD_EPI_2009 = cohort$Target_2009, CKD_EPI_2021 = cohort$Target_2021))
cat("\nTabla cruzada sin ponderar (subgrupo):\n"); print(with(cohort[cohort$subgrupo, ], table(CKD_EPI_2009 = Target_2009, CKD_EPI_2021 = Target_2021)))
cat("\nComponentes del desenlace (sin ponderar): eGFR<60 (2021) =", sum(cohort$eGFR_2021 < 60),
    "| eGFR<60 (2009) =", sum(cohort$eGFR_2009 < 60), "| UACR>=30 =", sum(cohort$UACR >= 30), "\n")
