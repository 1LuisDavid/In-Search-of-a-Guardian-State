#### Modelos e Hipótesis -----


Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica


library(tidyverse)
library(fixest)
library(lmtest)
library(sensemakr)
library(texreg)
library(modelsummary)
library(ggthemes)
library(hrbrthemes)

##### H1: El efecto de Backsliding sobre Attacks será positivo-----

df_h1 <-
  df %>% dplyr:: select(1,3,49, dem_backsliding, years_backsliding, regime, exec_respect_const, judicial_constraints, legislative_constraints, presidentialism, gini, rule_of_law, gdp_pc_2015usd, pubsec_corruption, regime_corruption, merit_criteria_admin, icrg_qog, edu_inequality, region) %>% 
  mutate(gdp_pc_2015usd = gdp_pc_2015usd/1000) %>% 
  mutate(attacks1 = 1-icrg_qog) %>% 
  mutate(attacks2 = 4-merit_criteria_admin) %>% 
  mutate(pop_power = 1-((judicial_constraints + legislative_constraints)/2))

df_h1 <-
  df_h1 %>%
  mutate(dem_backsliding = factor(dem_backsliding,
      levels = c("stable.dem", "d.backsliding"),
      labels = c("0", "1")))


df_h1 %>% dplyr::select(country, dem_backsliding) %>% 
  filter(dem_backsliding == 1) %>% 
  group_by(country) %>% count() %>% ungroup() %>% 
  summarise(total = sum(n))

summary(df_h1)

mod1 <-
  feols(attacks2 ~ 
          dem_backsliding + 
          pop_power +
          I(pop_power * pop_power) +
          gdp_pc_2015usd + 
          rule_of_law + 
          gini | country + year, 
        data = df_h1,
        vcov = "HC1") # se está controlando por DB_years por los efectos fijos y por la variable backsliding |||| si agrego judicial constraints es significativo al 90%

mod1


adjustmentSets(Backsliding_dag)



# Personalizar términos si quieres renombrarlos
coef_labels <- c(
  "dem_backsliding1" = "Democratic Backsliding",
  "pop_power" = "Populist Power",
  "I(pop_power * pop_power)" = "Populist Power Squared",
  "gdp_pc_2015usd" = "GDP per Capita",
  "rule_of_law" = "Rule of Law",
  "gini" = "Gini Index"
)

# Compute White/HC1 robust variance-covariance matrix


# Generate regression table with robust SEs
modelsummary(
  list("Model 1" = mod1),
  coef_map = coef_labels,
  statistic = "std.error",
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  # gof_omit = "IC$|Log$|F$|RMSE$",
  # gof_map = c(
  #   nobs = "N",
  #   r.squared = "R²",
  #   adj.r.squared = "Adjusted R²",
  #   within.r.squared = "Within R²",
  #   fe = "Fixed effects"
  # ),
  notes = c("All models include country and year fixed effects. Robust standard errors (White-HC1) reported."),
  title = "H1: Effects of Democratic Backsliding on Attacks",
  output = "regression_model1.docx"
)





#Pruebas

# Estimación sin robustez
summary(mod1, cluster = NULL)

# Estimación con errores robustos tipo White
summary(mod1, vcov = "white")



mod1.sensitivity <-
  sensemakr(model = mod1,
            treatment = "dem_backsliding1",
            benchmark_covariates = "rule_of_law",
            kd = 1:3,
            ky = 1:3)

summary(mod1.sensitivity)

plot(mod1.sensitivity)


df_h1 %>% 
  filter(gini > 0) %>% 
  dplyr:: select(attacks2, dem_backsliding) %>%
  
  ggplot(aes(x = dem_backsliding , y = attacks2 ))+
  geom_point(alpha = 0.1) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(alpha = 0.5) +
  theme_minimal()
  

assump_m1 <- as_tibble(c(1:622, 1))

assump_m1$fit <- fitted(mod1)       
assump_m1$resid <- resid(mod1)      


ggplot(assump_m1, aes(x = fit, y = resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth()+
  labs(title = "Residuos vs Valores ajustados",
       x = "Valores ajustados",
       y = "Residuos") +
  theme_minimal()

ggplot(assump_m1, aes(x = resid)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribución de residuos") +
  theme_minimal()

ggplot(assump_m1, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot de residuos") +
  theme_minimal()

dwtest(mod1)

library(broom)

tidy(mod1, conf.int = TRUE) %>%
  ggplot(aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Coeficientes con intervalos de confianza",
       x = "Estimación",
       y = "Variable") +
  theme_minimal()



library(car)
vif(mod1)  







##### H2: El efecto del Poder de las Organizaciones de la AP sobre Attacks hacia las burocracias está mediado por el VALOR POLÍTICO de las organizaciones de la Administración Pública------



df %>% dplyr::select(pubsec_corruption) %>%
  ggplot()+
  geom_histogram(aes(x = pubsec_corruption))

df_h2.1 <-
  df %>% dplyr:: select(1,3,8,49,  gov_effectiveness.x, merit_criteria_admin, icrg_qog, gini, rule_of_law, gdp_pc_2015usd, region, pubsec_corruption, polarization, years_backsliding) %>% 
  mutate(gdp_pc_2015usd = gdp_pc_2015usd/1000) %>% 
  mutate(attacks1 = 1-icrg_qog) %>% 
  mutate(attacks2 = 4-merit_criteria_admin) %>% 
  mutate(gov_squared = gov_effectiveness.x*gov_effectiveness.x) %>% 
  mutate(corrupt_squred = pubsec_corruption*pubsec_corruption)

df_h2.1 <-
  df_h2.1 %>% 
  filter(dem_backsliding == "d.backsliding")

df_h2.1 <-
  df_h2.1 %>%
  mutate(gov_effectiveness.x2 = gov_effectiveness.x)




##### H.2.1 Efecto de Political Value  sobre attacks2 (merit criteria admin) & fecto de Political value (corrupción) sobre attacks2 (merit criteria admin). -----


mod_p_value <- 
  feols(
    attacks2 ~  
      pubsec_corruption +
      corrupt_squred +
      gov_effectiveness.x +
      gov_squared +
      # polarization +
      rule_of_law + 
      gdp_pc_2015usd + 
      years_backsliding, 
    data = df_h2.1,
    vcov = "HC1"
  ) #aquí no agrego polarización porque es la corrupción la que afecta a la polarización y por lo tanto no es confounder

mod_p_value

adjustmentSets(p_value_dag)

df_h2.1$pubsec_corruption

df_h2.1 %>% 
  ggplot(aes(x = pubsec_corruption , y = attacks2))+
  geom_point()

#### Pruebas 
coef_labels_p_value <- c(
  "pubsec_corruption" = "Agencies' Political Value",
  "corrupt_squred"  = "Agencies' Political Value Squared",
  "gov_effectiveness.x" = "Agencies' Power",
  "gov_squared" = "Agencies' Power Squared",
  "years_backsliding" = "Years under Backsliding",
  "gdp_pc_2015usd" = "GDP per Capita",
  "rule_of_law" = "Rule of Law"
)

# Compute White/HC1 robust variance-covariance matrix

# Estimación sin robustez
summary(mod_p_value, cluster = NULL)

# Estimación con errores robustos tipo White
summary(mod_p_value, vcov = "white")


# Generate regression table with robust SEs
modelsummary(
  list("Model p_value" = mod_p_value),
  coef_map = coef_labels_p_value,
  statistic = "std.error",
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  # gof_omit = "IC$|Log$|F$|RMSE$",
  # gof_map = c(
  #   nobs = "N",
  #   r.squared = "R²",
  #   adj.r.squared = "Adjusted R²",
  #   within.r.squared = "Within R²",
  #   fe = "Fixed effects"
  # ),
  notes = c("Robust standard errors (White-HC1) reported."),
  title = "H?: The Effect of Agencie's Political Value on Attacks",
  output = "regression_model_p_value_bis.docx"
)



#plot prediction
library(marginaleffects)

plot_predictions(mod_p_value, condition = list("pubsec_corruption"))

#Pruebas
mod3.sensitivity <-
  sensemakr(model = mod_p_value,
            treatment = "corrupt_squred", # corrupt_squred
            benchmark_covariates = "gov_effectiveness.x",
            kd = 1:3,
            ky = 1:3)

plot(mod3.sensitivity)

summary(mod3.sensitivity)

df_h2.1 %>%
  # filter(country == "Venezuela") %>%
  ggplot(aes(x = pubsec_corruption , y = attacks1  ))+
  geom_point(alpha = 1) +
  geom_jitter()+
  geom_smooth()+
  # facet_wrap(~country, scales = "free_y" )+
  theme_minimal()


assump_m1 <- as_tibble(c(1:153, 1))

assump_m1$fit <- fitted(mod_p_value)       
assump_m1$resid <- resid(mod_p_value)      


ggplot(assump_m1, aes(x = fit, y = resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuos vs Valores ajustados",
       x = "Valores ajustados",
       y = "Residuos") +
  geom_smooth()+
  theme_minimal()

ggplot(assump_m1, aes(x = resid)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribución de residuos") +
  theme_minimal()

ggplot(assump_m1, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot de residuos") +
  theme_minimal()


library(broom)

tidy(mod_p_value, conf.int = TRUE) %>%
  ggplot(aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Coeficientes con intervalos de confianza",
       x = "Estimación",
       y = "Variable") +
  theme_minimal()



###### Modelo Poder de las agencias (Gov Effectiveness) -----
mod_p_agency <- 
  feols(
    attacks2 ~
      gov_effectiveness.x +
      gov_squared +
      polarization +
      rule_of_law + 
      gdp_pc_2015usd + 
      years_backsliding,
    data = df_h2.1,
    vcov = "HC1"
    )


mod_p_agency

adjustmentSets(A_power_dag) 

df_h2.1 %>% 
ggplot(aes(x = gov_effectiveness.x , y = attacks2))+
  geom_point()

#### Tests

coef_labels_p_agency <- c(
  "gov_effectiveness.x" = "Agencies' Power",
  "gov_squared" = "Agencies' Power Squared",
  
  "polarization" = "Polarization",
  "years_backsliding" = "Years under Backsliding",
  "gdp_pc_2015usd" = "GDP per Capita",
  "rule_of_law" = "Rule of Law"
)

# Compute White/HC1 robust variance-covariance matrix

# Estimación sin robustez
summary(mod_p_agency, cluster = NULL)

# Estimación con errores robustos tipo White
summary(mod_p_agency, vcov = "white")


# Generate regression table with robust SEs
modelsummary(
  list("Model p_agency" = mod_p_agency),
  coef_map = coef_labels_p_agency,
  statistic = "std.error",
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  # gof_omit = "IC$|Log$|F$|RMSE$",
  # gof_map = c(
  #   nobs = "N",
  #   r.squared = "R²",
  #   adj.r.squared = "Adjusted R²",
  #   within.r.squared = "Within R²",
  #   fe = "Fixed effects"
  # ),
  notes = c("Robust standard errors (White-HC1) reported."),
  title = "H?: The Effect of Agencie's Poower on Attacks",
  output = "regression_model_p_agency_bis.docx"
)



#plot prediction
library(marginaleffects)

plot_predictions(mod_p_agency, condition = list("gov_effectiveness.x"))

#Pruebas
mod3.sensitivity <-
  sensemakr(model = mod_p_agency,
            treatment = "gov_effectiveness.x", # gov_squared
            benchmark_covariates = "years_backsliding",
            kd = 1:3,
            ky = 1:3)

plot(mod3.sensitivity)

summary(mod3.sensitivity)

df_h2.1 %>%
  # filter(country == "Venezuela") %>%
  ggplot(aes(x = gov_effectiveness.x , y = attacks1  ))+
  geom_point(alpha = 1) +
  geom_jitter()+
  geom_smooth()+
  # facet_wrap(~country, scales = "free_y" )+
  theme_minimal()


assump_m1 <- as_tibble(c(1:153, 1))

assump_m1$fit <- fitted(mod_p_agency)       
assump_m1$resid <- resid(mod_p_agency)      


ggplot(assump_m1, aes(x = fit, y = resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuos vs Valores ajustados",
       x = "Valores ajustados",
       y = "Residuos") +
  geom_smooth()+
  theme_minimal()

ggplot(assump_m1, aes(x = resid)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribución de residuos") +
  theme_minimal()

ggplot(assump_m1, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot de residuos") +
  theme_minimal()


library(broom)

tidy(mod_p_agency, conf.int = TRUE) %>%
  ggplot(aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Coeficientes con intervalos de confianza",
       x = "Estimación",
       y = "Variable") +
  theme_minimal()



###### H2.2: El Efecto Natural Indirecto Agency power -> Attacks será significativo y positivo || Obtener (NIE / Total Efect) -----

#### Efectos naturales finales 


#### 1. CENTRAR VARIABLES ---

# Guardamos valor original antes de centrar (para usarlo como gov_value)
df_h2.1 <- df_h2.1 %>%
  mutate(gov_effectiveness.x2 = gov_effectiveness.x)

# Calcular términos cuadráticos sin centrar
df_h2.1 <- 
  df_h2.1 %>%
  mutate(
    corrupt_x_effect = gov_effectiveness.x * pubsec_corruption,
    gov_squared_raw = gov_effectiveness.x2^2,
    corrupt_squared_raw = pubsec_corruption^2
  )

# Guardar medias de los términos cuadráticos sin centrar
gov_squared_mean <- mean(df_h2.1$gov_squared_raw, na.rm = TRUE)

corrupt_squared_mean <- mean(df_h2.1$corrupt_squared_raw, na.rm = TRUE)

# Centramos todas las variables necesarias
df_h2.1 <- 
  df_h2.1 %>%
  mutate(
    gov_effectiveness.x = scale(gov_effectiveness.x2, center = TRUE, scale = FALSE),
    pubsec_corruption = scale(pubsec_corruption, center = TRUE, scale = FALSE),
    corrupt_x_effect = scale(corrupt_x_effect, center = TRUE, scale = FALSE),
    gov_squared = gov_squared_raw - gov_squared_mean,
    corrupt_squred = corrupt_squared_raw - corrupt_squared_mean
  )

#### 2. MODELO DEL MEDIADOR: M ~ X + Z ---

mod_mediator <- 
  
  feols(
    pubsec_corruption ~ gov_effectiveness.x +
      gov_squared +
      # polarization +
      rule_of_law + 
      gdp_pc_2015usd + 
      years_backsliding,
    
    data = df_h2.1,
  vcov = "HC1")

df_h2.1 %>% 
  ggplot(aes(y = attacks2, x = pubsec_corruption))+
  geom_point()


#### 3. MODELO DEL RESULTADO: Y ~ X + M + Z ---

mod_outcome <- 
  
  feols(
    attacks2 ~ gov_effectiveness.x +
      gov_squared + 
      pubsec_corruption + 
      corrupt_squred +
      polarization + rule_of_law + gdp_pc_2015usd + years_backsliding,
    
    data = df_h2.1,
  vcov = "HC1")

df_h2.1 %>%
ggplot(aes(x = gov_effectiveness.x ,y = attacks2 ))+
  geom_point()

#### 4. DEFINIR VALORES DE X: tratamiento x y referencia x* ---

gov_value <- quantile(df_h2.1$gov_effectiveness.x2, 0.75, na.rm = TRUE)  # x
gov_mean <- mean(df_h2.1$gov_effectiveness.x2, na.rm = TRUE)            # x*

#### 5. PREDECIR m(x) y m(x*): valores contrafactuales del mediador ---

# m(x) E[M∣do(X=x)]
df_mx <- 
  df_h2.1 %>%
  mutate(gov_effectiveness.x = gov_value - gov_mean)

df_mx$mediator_mx <- 
  predict(mod_mediator, newdata = df_mx)

# m(x*) E[M∣do(X=x*)]
df_mxstar <- 
  df_h2.1 %>%
  mutate(gov_effectiveness.x = 0)

df_mxstar$mediator_mxstar <- 
  predict(mod_mediator, newdata = df_mxstar)

#### 6. NDE: Y(x, m(x*)) - Y(x*, m(x*)) ---

# Y(x, m(x*))
df_pred_nde <- 
  df_h2.1 %>%
  mutate(
    gov_effectiveness.x = gov_value - gov_mean,
    pubsec_corruption = df_mxstar$mediator_mxstar,
    gov_squared = (gov_effectiveness.x + gov_mean)^2 - gov_squared_mean,
    corrupt_squred = pubsec_corruption^2 - corrupt_squared_mean
  )

# #E[Y∣do(X=x,M=m(x∗))]
df_pred_nde$nde_pred <- 
  predict(mod_outcome, newdata = df_pred_nde)

# Y(x*, m(x*)). Baseline
df_pred_base <- 
  df_h2.1 %>%
  mutate(
    gov_effectiveness.x = 0,
    pubsec_corruption = df_mxstar$mediator_mxstar,
    gov_squared = (gov_effectiveness.x + gov_mean)^2 - gov_squared_mean,
    corrupt_squred = pubsec_corruption^2 - corrupt_squared_mean
  )
#E[Y∣do(X=x*,M=m(x∗))]. --- Baseline
df_pred_base$baseline_pred <- 
  predict(mod_outcome, newdata = df_pred_base)

#### 7. NIE: Y(x*, m(x)) - Y(x*, m(x*)) ---

# Y(x*, m(x))
df_pred_nie <- 
  df_h2.1 %>%
  mutate(
    gov_effectiveness.x = 0,
    pubsec_corruption = df_mx$mediator_mx,
    gov_squared = (gov_effectiveness.x + gov_mean)^2 - gov_squared_mean,
    corrupt_squred = pubsec_corruption^2 - corrupt_squared_mean
  )


# E[Y∣do(X=x∗,M=m(x))]
df_pred_nie$nie_pred <- predict(mod_outcome, newdata = df_pred_nie)

#### 8. CALCULAR EFECTOS ---

NDE <- mean(df_pred_nde$nde_pred - df_pred_base$baseline_pred, na.rm = TRUE)
NIE <- mean(df_pred_nie$nie_pred - df_pred_base$baseline_pred, na.rm = TRUE)
TE <- NDE + NIE

mean(df_pred_nie$nie_pred, na.rm = TRUE)

#### 9. MOSTRAR RESULTADOS ---

cat("Efecto Directo Natural (NDE):", round(NDE, 4), "\n")
cat("Efecto Indirecto Natural (NIE):", round(NIE, 4), "\n")
cat("Efecto Total:", round(TE, 4), "\n")


#### Intervalos de confianza ---

# Número de simulaciones bootstrap
set.seed(1234)  
n_boot <- 1000

# Crear vectores vacíos para guardar los resultados
NDE_vec <- numeric(n_boot)
NIE_vec <- numeric(n_boot)
TE_vec <- numeric(n_boot)

# Bootstrap loop
for (i in 1:n_boot) {
  
  # 1. Resamplear datos con reemplazo
  df_boot <- df_h2.1[sample(1:nrow(df_h2.1), replace = TRUE), ]
  
  # 2. Recalcular medias originales
  gov_mean_boot <- mean(df_boot$gov_effectiveness.x2, na.rm = TRUE)
  gov_value_boot <- quantile(df_boot$gov_effectiveness.x2, 0.75, na.rm = TRUE)
  
  gov_squared_mean_boot <- mean(df_boot$gov_squared_raw, na.rm = TRUE)
  corrupt_squared_mean_boot <- mean(df_boot$corrupt_squared_raw, na.rm = TRUE)
  
  # 3. Ajustar modelos
  mod_m_boot <- feols(
    pubsec_corruption ~ gov_effectiveness.x +
      # polarization + 
      gov_squared +
      rule_of_law + gdp_pc_2015usd + years_backsliding,
    data = df_boot
  )
  
  mod_y_boot <- feols(
    attacks2 ~ gov_effectiveness.x +
      gov_squared + pubsec_corruption + corrupt_squred +
      polarization + rule_of_law + gdp_pc_2015usd + years_backsliding,
    data = df_boot
  )
  
  # 4. Predicción de m(x) y m(x*)
  df_mx_boot <- df_boot %>%
    mutate(gov_effectiveness.x = gov_value_boot - gov_mean_boot)
  df_mx_boot$mediator_mx <- predict(mod_m_boot, newdata = df_mx_boot)
  
  df_mxstar_boot <- df_boot %>%
    mutate(gov_effectiveness.x = 0)
  df_mxstar_boot$mediator_mxstar <- predict(mod_m_boot, newdata = df_mxstar_boot)
  
  # 5. Predicciones de Y contrafactuales
  
  # Y(x, m(x*))
  df_pred_nde_boot <- df_boot %>%
    mutate(
      gov_effectiveness.x = gov_value_boot - gov_mean_boot,
      pubsec_corruption = df_mxstar_boot$mediator_mxstar,
      gov_squared = (gov_effectiveness.x + gov_mean_boot)^2 - gov_squared_mean_boot,
      corrupt_squred = pubsec_corruption^2 - corrupt_squared_mean_boot
    )
  pred_nde <- predict(mod_y_boot, newdata = df_pred_nde_boot)
  
  # Y(x*, m(x*))
  df_pred_base_boot <- df_boot %>%
    mutate(
      gov_effectiveness.x = 0,
      pubsec_corruption = df_mxstar_boot$mediator_mxstar,
      gov_squared = (gov_effectiveness.x + gov_mean_boot)^2 - gov_squared_mean_boot,
      corrupt_squred = pubsec_corruption^2 - corrupt_squared_mean_boot
    )
  pred_base <- predict(mod_y_boot, newdata = df_pred_base_boot)
  
  # Y(x*, m(x))
  df_pred_nie_boot <- df_boot %>%
    mutate(
      gov_effectiveness.x = 0,
      pubsec_corruption = df_mx_boot$mediator_mx,
      gov_squared = (gov_effectiveness.x + gov_mean_boot)^2 - gov_squared_mean_boot,
      corrupt_squred = pubsec_corruption^2 - corrupt_squared_mean_boot
    )
  pred_nie <- predict(mod_y_boot, newdata = df_pred_nie_boot)
  
  # 6. Guardar efectos promedio
  NDE_vec[i] <- mean(pred_nde - pred_base, na.rm = TRUE)
  NIE_vec[i] <- mean(pred_nie - pred_base, na.rm = TRUE)
  TE_vec[i] <- NDE_vec[i] + NIE_vec[i]
}

# 7. Calcular intervalos de confianza
ci_NDE <- quantile(NDE_vec, c(0.05, 0.95), na.rm = TRUE)
ci_NIE <- quantile(NIE_vec, c(0.05, 0.95), na.rm = TRUE)
ci_TE <- quantile(TE_vec, c(0.05, 0.95), na.rm = TRUE)

# 8. Mostrar resultados
cat("Efecto Directo Natural (NDE):", round(NDE, 4), 
    "IC 90% [", round(ci_NDE[1], 4), ",", round(ci_NDE[2], 4), "]\n")
cat("Efecto Indirecto Natural (NIE):", round(NIE, 4), 
    "IC 90% [", round(ci_NIE[1], 4), ",", round(ci_NIE[2], 4), "]\n")
cat("Efecto Total (TE):", round(TE, 4), 
    "IC 90% [", round(ci_TE[1], 4), ",", round(ci_TE[2], 4), "]\n")



# Creamos un dataframe con los resultados
df_effects <- data.frame(
  Efecto = c("NDE", "NIE", "TE"),
  Estimación = c(NDE, NIE, TE),
  IC_inf = c(ci_NDE[1], ci_NIE[1], ci_TE[1]),
  IC_sup = c(ci_NDE[2], ci_NIE[2], ci_TE[2])
)

# g_natural_attck1 <-
ggplot(df_effects, aes(x = Efecto, y = Estimación, label = round(Estimación, 3))) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.1, color = "gray30") +
  geom_text(aes(vjust = -1))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip()+
  # theme_tufte()+
  theme_pander()+
  labs(
    title = "Graph 3: Estimations of Natural Effects",
    y = "Effects on Attacks to the Merit Criteria for Appointment",
    x = NULL,
    caption = "CI at 90%"
    )

g_final <-
g_natural_attck2/g_natural_attck1



##### H3 El efecto de poder del populista será positivo ------


df_h3 <-
  df %>% dplyr::select(1,3,8,49, icrg_qog, region, polarization,  gov_effectiveness.x, regime, exec_respect_const, judicial_constraints, legislative_constraints, presidentialism, gini, rule_of_law, gdp_pc_2015usd, pubsec_corruption, regime_corruption, merit_criteria_admin,  years_backsliding) %>% 
  mutate(gdp_pc_2015usd = gdp_pc_2015usd/1000) %>% 
  mutate(attacks1 = 1-icrg_qog) %>%
  mutate(attacks2 = 4-merit_criteria_admin) %>% 
  mutate(pop_power = 1-((judicial_constraints + legislative_constraints)/2))

df_h3 <-
  df_h3 %>% 
  filter(dem_backsliding == "d.backsliding")




mod3 <-
  feols(attacks2 ~ 
          pop_power + 
          I(pop_power^2) + 
          rule_of_law + 
          gdp_pc_2015usd + 
          # polarization +
          years_backsliding | country, 
        data = df_h3,
        vcov = "HC1") #Con att2 es significativo, con att 1 los coeficientes tienen diferentes signos

mod3

df_h3 %>%
  # filter(attacks1 < 0.6) %>% 
  ggplot(aes(x = pop_power, y =attacks1, label = country))+
  geom_text()+
  geom_smooth()

adjustmentSets(pop_power_dag)

# Personalizar términos si quieres renombrarlos
coef_labels3 <- c(
  "pop_power" = "Populist Power",
  "I(pop_power^2)"  = "Populist Power Squared",
  "years_backsliding" = "Years under Backsliding",
  "gdp_pc_2015usd" = "GDP per Capita",
  "rule_of_law" = "Rule of Law"
)

# Compute White/HC1 robust variance-covariance matrix


# Generate regression table with robust SEs
modelsummary(
  list("Model 3" = mod3),
  coef_map = coef_labels3,
  statistic = "std.error",
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  # gof_omit = "IC$|Log$|F$|RMSE$",
  # gof_map = c(
  #   nobs = "N",
  #   r.squared = "R²",
  #   adj.r.squared = "Adjusted R²",
  #   within.r.squared = "Within R²",
  #   fe = "Fixed effects"
  # ),
  notes = c("All models include country and year fixed effects. Robust standard errors (White-HC1) reported."),
  title = "H3: The Effect of Populist's Power on Attacks",
  output = "regression_model3_bis.docx"
)



#plot prediction
library(marginaleffects)

plot_predictions(mod3, condition = list("pop_power"))

#Pruebas
mod3.sensitivity <-
sensemakr(model = mod3,
        treatment = "pop_power", # I(pop_power^2)
        benchmark_covariates = "gdp_pc_2015usd",
          kd = 1:3,
          ky = 1:3)

plot(mod3.sensitivity)

summary(mod3.sensitivity)
df_h3 %>%
  # filter(country == "Venezuela") %>%
  ggplot(aes(x = pop_power , y = attacks1  ))+
  geom_point(alpha = 1) +
  geom_jitter()+
  geom_smooth()+
  theme_minimal()


assump_m1 <- as_tibble(c(1:179, 1))

assump_m1$fit <- fitted(mod3)       
assump_m1$resid <- resid(mod3)      

assump_m2 <- as_tibble(c(1:173, 1))

assump_m2$fit <- fitted(mod3)       
assump_m2$resid <- resid(mod3)


ggplot(assump_m2, aes(x = fit, y = resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuos vs Valores ajustados",
       x = "Valores ajustados",
       y = "Residuos") +
  geom_smooth()+
  theme_minimal()

ggplot(assump_m2, aes(x = resid)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribución de residuos") +
  theme_minimal()

qq_mod3_att2 <-
ggplot(assump_m2, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot de residuos Attack 2") +
  theme_minimal()

qq_mod3_att2/qq_mod3_att1

library(broom)

tidy(mod3, conf.int = TRUE) %>%
  ggplot(aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Coeficientes con intervalos de confianza",
       x = "Estimación",
       y = "Variable") +
  theme_minimal()




