####### Obtaining variables  -------


Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica

library(tidyverse)
library(readr)
library(haven)
library(purrr)

pop_leaders <- read_dta("02_data/PLE_panel2.dta")

qog <- read_csv("02_data/qog_bas_ts_jan25.csv")

vdem <- readRDS("~/Desktop/comps_project/02_data/V-Dem-CY-FullOthers-v15_rds/V-Dem-CY-Full+Others-v15.rds")


names(qog)

qog <-
  qog %>% 
  select(1:3,6, "cspf_sfi", "fh_fog", "icrg_qog", "iiag_srol", "wbgi_gee", "lis_gini", "top_top1_income_share", "top_top10_income_share", "wdi_tacpsr", "wdi_gdpcapcon2015", "wdi_homicides", "wdi_tacpsr", "wdi_spr"    )

names(vdem)

df <-
vdem %>% 
  as_tibble() %>% 
  select(
    1:4, 
    v2x_libdem, 
    v2x_regime, 
    v2x_jucon,       
    v2xlg_legcon,    
    v2pscohesv,      
    v2psnatpar,      
    v2pssunpar,      
    v2exrescon,      
    v2exbribe,       
    v2exembez,       
    v2excrptps,      
    v2exthftps,      
    v2exapup,        
    v2exapupap,      
    v2exdfpphs,      
    v2exdfvthg,      
    v2lgotovst,      
    v2jupurge,       
    v2jupoatck,      
    v2jureview,      
    v2clrspct,       
    v2stcritrecadm,  
    v2cacamps,       
    v2xnp_pres,      
    v2xnp_regcorr,   
    v2x_execorr,     
    v2x_pubcorr,     
    v2x_rule,        
    e_wbgi_gee,      
    e_bnr_dem,       
    e_peedgini,      
    e_regionpol,     
    e_gdppc,
    e_peaveduc
  ) %>% 
  
  rename(
    lib_dem_index = v2x_libdem,
    regime = v2x_regime,
    judicial_constraints = v2x_jucon,
    legislative_constraints = v2xlg_legcon,
    party_cohesion = v2pscohesv,
    nat_party_control = v2psnatpar,
    subnat_party_control = v2pssunpar,
    exec_respect_const = v2exrescon,
    exec_bribery = v2exbribe,
    exec_embezzlement = v2exembez,
    pubsec_bribery = v2excrptps,
    pubsec_theft = v2exthftps,
    exec_appointed_upper = v2exapup,
    exec_approval_upper = v2exapupap,
    hos_propose_law = v2exdfpphs,
    hog_veto_power = v2exdfvthg,
    exec_oversight = v2lgotovst,
    judicial_purges = v2jupurge,
    judicial_attacks = v2jupoatck,
    judicial_review = v2jureview,
    impartial_admin = v2clrspct,
    merit_criteria_admin = v2stcritrecadm,
    polarization = v2cacamps,
    presidentialism = v2xnp_pres,
    regime_corruption = v2xnp_regcorr,
    exec_corruption = v2x_execorr,
    pubsec_corruption = v2x_pubcorr,
    rule_of_law = v2x_rule,
    gov_effectiveness = e_wbgi_gee,
    dem_breakdown = e_bnr_dem,
    edu_inequality = e_peedgini,
    region = e_regionpol,
    gdp_per_capita = e_gdppc,
    education_e = e_peaveduc
  ) 

#### Pop leaders after 1990 ---

# pop_leaders <-
#   pop_leaders %>% 
#   filter(year >= 1990)

df <-
  df %>% 
  rename(iso = country_text_id) %>% 
  rename(country = country_name) %>%   
  filter(year >= 1980) %>% select(-3)



df <-
  df %>%
  left_join(pop_leaders, 
            by = c("country", "iso", "year")) %>% 
  select(1:5,39,40,41, 6:38)



df <-
  df %>% 
  # filter(country == "Argentina") %>% 
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    pop_shift = lag(pop, default = 0),
    start_new = pop == 1 & (is.na(pop_shift) | pop_shift == 0),
    pop_period = ifelse(pop == 1, cumsum(start_new), NA_integer_)
  ) %>%
  select(-pop_shift, -start_new) %>%
  ungroup() %>% 
  mutate(pop_period = ifelse(pop_period == 1, "Primer",
                             ifelse(pop_period == 2, "Segundo",
                                    ifelse(pop_period == 3, "tercero",
                                           ifelse(pop_period == 4, "cuarto", NA))))) %>% 
  # select(1:4,6, 41) %>% 
  group_by(country, pop_period) %>% 

mutate(v_max_pop = max(lib_dem_index[pop == 1], na.rm = TRUE)) %>%
  mutate(v_min_pop = min(lib_dem_index[pop == 1], na.rm = TRUE)) %>%
  mutate(dem_decreased = v_max_pop - v_min_pop) %>%
  ungroup() %>% 
  mutate(dem_backsliding = ifelse(dem_decreased > 0, "d.backsliding", "stable.dem")) %>% 
  filter(year >= 1990) %>% 
  filter(regime >= 2) %>% 
  select(1:6, 45,46, 7:44) %>% 
  mutate(pop = replace_na(pop, 0)) %>%
  mutate(dem_decreased = ifelse(dem_decreased < 0, 0, ifelse(dem_decreased > 1, 0, dem_decreased)))


df %>% count(dem_backsliding)


names(df)

qog <-
  qog %>%
  rename(
    country = cname_qog,
    iso = ccodealp,
    state_frag_indx = cspf_sfi,
    func_gov = fh_fog,
    icrg_qog = icrg_qog,
    rule_law_score = iiag_srol,
    gov_effectiveness = wbgi_gee,
    gini = lis_gini,
    top1_income_share = top_top1_income_share,
    transparency_rating = wdi_tacpsr,
    gdp_pc_2015usd = wdi_gdpcapcon2015,
    homicides_100k = wdi_homicides,
    social_protection_rating = wdi_spr
  )

# qog$gdp_pc_2015usd

# qog <-
# qog %>% 
#   rename(country = cname) %>% 
#   rename(iso = ccodealp) 
  
df <-
  df %>%
  left_join(qog, 
            by = c("country", "iso", "year"))


names(df)

###### Variables adicionales de QoG ------

#1
percentile_ratio_90_10 <- 
  
  read_delim("02_data/90-10 percentile ratio qogdata_10_07_2025.csv", 
                                                         delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                              ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                              ccodecow = col_skip(), cname_year = col_skip(), 
                                                                                                              ccodealp_year = col_skip()), trim_ws = TRUE)

percentile_ratio_90_10 <-
  percentile_ratio_90_10 %>% 
  rename(
    country = cname,
    iso = ccodealp,
    ratio_90_10 = lis_pr9010)

#2 

anticorruption_policy <- 
  read_delim("02_data/anticorruption policy qogdata_10_07_2025.csv", 
                                                       delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                            ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                            ccodecow = col_skip(), cname_year = col_skip(), 
                                                                                                            ccodealp_year = col_skip()), trim_ws = TRUE)

anticorruption_policy <-
  anticorruption_policy %>% 
  rename(
    country = cname,
    iso = ccodealp,
    anticorr_policy = bti_acp)

#3

aproval_of_democracy <- 
  read_delim("02_data/aproval of democracy qogdata_10_07_2025.csv", 
                                                      delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                           ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                           ccodecow = col_skip(), cname_year = col_skip(), 
                                                                                                           ccodealp_year = col_skip()), trim_ws = TRUE)

aproval_of_democracy <-
  aproval_of_democracy %>% 
  rename(
    country = cname,
    iso = ccodealp,
    aprov_dem = bti_aod)

#4


basic_admon <- 
  read_delim("02_data/Basic administration qogdata_09_07_2025.csv", 
                                                      delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                           ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                           ccodecow = col_skip(), cname_year = col_skip(), 
                                                                                                           ccodealp_year = col_skip()), trim_ws = TRUE)

basic_admon <-
  basic_admon %>% 
  rename(
    country = cname,
    iso = ccodealp,
    basic_admon = bti_ba)

#5 
Commitment_to_Democratic_Institutions <- 
  read_delim("02_data/Commitment to Democratic Institutions qogdata_09_07_2025.csv", 
                                                                       delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                                            ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                                            ccodecow = col_skip(), cname_year = col_skip(), 
                                                                                                                            ccodealp_year = col_skip()), trim_ws = TRUE)


Commitment_to_Democratic_Institutions <- 
  Commitment_to_Democratic_Institutions %>% 
  rename(
    country = cname,
    iso = ccodealp,
    commitment_demo_inst = bti_cdi)

#6 

confidence_civil_services <- 
  read_delim("02_data/confidence civil services qogdata_09_07_2025.csv", 
                                                           delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                                ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                                ccodecow = col_skip(), cname_year = col_skip(), 
                                                                                                                ccodealp_year = col_skip()), trim_ws = TRUE)
  
confidence_civil_services <-
  confidence_civil_services %>% 
  rename(
    country = cname,
    iso = ccodealp,
    confidence_cs = wvs_confcs)


#7

quality_of_public_administration_rating <- read_delim("02_data/CPIA quality of public administration rating qogdata_10_07_2025.csv", 
                                                                              delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                                                   ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                                                   ccodecow = col_skip(), cname_year = col_skip(), 
                                                                                                                                   ccodealp_year = col_skip()), trim_ws = TRUE)


quality_of_public_administration_rating <-
  quality_of_public_administration_rating %>% 
  rename(
    country = cname,
    iso = ccodealp,
    wb_quality_PA = wdi_qpubadm)


#8

Economy_status_qog <- 
  read_delim("02_data/Economy status qogdata_10_07_2025.csv", 
                                                delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                     ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                     ccodecow = col_skip(), cname_year = col_skip(), 
                                                                                                     ccodealp_year = col_skip()), trim_ws = TRUE)

Economy_status_qog <-
  Economy_status_qog %>% 
  rename(
    country = cname,
    iso = ccodealp,
    eco_status_qog = bti_mes)

#9

equal_opportunity_qog <- 
  read_delim("02_data/equal opportunity qogdata_10_07_2025.csv", 
                                                   delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                        ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                        ccodecow = col_skip(), cname_year = col_skip(), 
                                                                                                        ccodealp_year = col_skip()), trim_ws = TRUE)


equal_opportunity_qog <-
  equal_opportunity_qog %>% 
  rename(
    country = cname,
    iso = ccodealp,
    equal_opp_qog = bti_eo)


#10

Governance_index_qog <- 
  read_delim("02_data/Governance index qogdata_09_07_2025.csv", 
                                                  delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                       ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                       ccodecow = col_skip(), cname_year = col_skip(), 
                                                                                                       ccodealp_year = col_skip()), trim_ws = TRUE)

Governance_index_qog <-
  Governance_index_qog %>% 
  rename(
    country = cname,
    iso = ccodealp,
    governance_qog = bti_gi)


#11

Governance_performance_qog <- 
  read_delim("02_data/Governance performance qogdata_09_07_2025.csv", 
                                                        delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                             ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                             ccodecow = col_skip(), cname_year = col_skip(), 
                                                                                                             ccodealp_year = col_skip()), trim_ws = TRUE)


Governance_performance_qog <- 
  Governance_performance_qog %>% 
  rename(
    country = cname,
    iso = ccodealp,
    governance_perf_qog = bti_gp)

#12

Independt_judiciary_qog <- 
  read_delim("02_data/Independt judiciary qogdata_10_07_2025.csv", 
                                                     delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                          ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                          ccodecow = col_skip(), cname_year = col_skip(), 
                                                                                                          ccodealp_year = col_skip()), trim_ws = TRUE)



Independt_judiciary_qog <-
  Independt_judiciary_qog %>% 
rename(
  country = cname,
  iso = ccodealp,
  indep_judiciary_qog = bti_ij)

#13

meritocratic_recruitment_of_civil_servants_mentiones_in_the_constitution_qogdata <- read_delim("02_data/meritocratic recruitment of civil servants mentiones in the constitution qogdata_09_07_2025.csv", 
                                                                                                          delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                                                                               ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                                                                               ccodecow = col_skip(), cname_year = col_skip(), 
                                                                                                                                                               ccodealp_year = col_skip()), trim_ws = TRUE)


meritocratic_recruitment_of_civil_servants_mentiones_in_the_constitution_qogdata <-
  meritocratic_recruitment_of_civil_servants_mentiones_in_the_constitution_qogdata %>% 
  rename(
    country = cname,
    iso = ccodealp,
    meritocratic = ccp_civil)

meritocratic_rec <- meritocratic_recruitment_of_civil_servants_mentiones_in_the_constitution_qogdata

#14
poverty_gap_qog <- 
  read_delim("02_data/poverty gap qogdata_10_07_2025.csv", 
                                             delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                  ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                  ccodecow = col_skip(), cname_year = col_skip(), 
                                                                                                  ccodealp_year = col_skip()), trim_ws = TRUE)


  poverty_gap_qog <- 
    poverty_gap_qog %>%
  mutate(wdi_povgap365 = str_replace(wdi_povgap365, ",", "."),
         wdi_povgap365 = as.numeric(wdi_povgap365)) %>% 
    rename(
      country = cname,
      iso = ccodealp,
      poverty_gap = wdi_povgap365)

#15

  prosecution_of_office_abuse_qog <- 
    read_delim("02_data/prosecution of office abuseqogdata_09_07_2025.csv", 
                                                              delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                                   ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                                   ccodecow = col_skip(), cname_year = col_skip(), 
                                                                                                                   ccodealp_year = col_skip()), trim_ws = TRUE)

prosecution_of_office_abuse_qog <-
  prosecution_of_office_abuse_qog %>% 
  rename(
    country = cname,
    iso = ccodealp,
    prosecution_off_abuse = bti_poa)
  

#16

Separation_of_powers_qog <- 
  read_delim("02_data/Separation of powers_qogdata_09_07_2025.csv", 
                                                      delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                           ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                           ccodealp_year = col_skip()), trim_ws = TRUE)


Separation_of_powers_qog <-
  Separation_of_powers_qog %>% 
  rename(
    country = cname,
    iso = ccodealp,
    separation_of_powers = bti_sop)

#17

socio_economic_barriers_qogdata_09_07_2025_2 <- read_delim("02_data/socio-economic barriers qogdata_09_07_2025-2.csv", 
                                                           delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                                ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                                ccodecow = col_skip(), cname_year = col_skip(), 
                                                                                                                ccodealp_year = col_skip()), trim_ws = TRUE)

socio_eco_barriers_qog <-
socio_economic_barriers_qogdata_09_07_2025_2 %>% 
  rename(
    country = cname,
    iso = ccodealp,
    socioeco_barriers = bti_seb)

#18
welfare_regime_qog <- 
  read_delim("02_data/welfare regime qogdata_09_07_2025.csv", 
                                                delim = ";", escape_double = FALSE, col_types = cols(ccode = col_skip(), 
                                                                                                     ccode_qog = col_skip(), cname_qog = col_skip(), 
                                                                                                     ccodecow = col_skip(), cname_year = col_skip(), 
                                                                                                     ccodealp_year = col_skip()), trim_ws = TRUE)

welfare_regime_qog <-
  welfare_regime_qog %>% 
  rename(
    country = cname,
    iso = ccodealp,
    welfare_regime = bti_wr)



#### Juntar todo en un df ----

df <-
  df %>%
  left_join(percentile_ratio_90_10, by = c("country", "year", "iso")) %>%
  left_join(anticorruption_policy, by = c("country", "year", "iso")) %>% 
  left_join(aproval_of_democracy, by = c("country", "year", "iso")) %>% 
  left_join(basic_admon, by = c("country", "year", "iso")) %>% 
  left_join(Commitment_to_Democratic_Institutions, by = c("country", "year", "iso")) %>% 
  left_join(confidence_civil_services, by = c("country", "year", "iso")) %>% 
  left_join(quality_of_public_administration_rating, by = c("country", "year", "iso")) %>% 
  left_join(Economy_status_qog, by = c("country", "year", "iso")) %>% 
  left_join(equal_opportunity_qog, by = c("country", "year", "iso")) %>% 
  left_join(Governance_index_qog, by = c("country", "year", "iso")) %>% 
  left_join(Governance_performance_qog, by = c("country", "year", "iso")) %>% 
  left_join(Independt_judiciary_qog, by = c("country", "year", "iso")) %>% 
  left_join(meritocratic_rec, by = c("country", "year", "iso")) %>% 
  left_join(poverty_gap_qog, by = c("country", "year", "iso")) %>% 
  left_join(prosecution_of_office_abuse_qog, by = c("country", "year", "iso")) %>%
  left_join(Separation_of_powers_qog, by = c("country", "year", "iso")) %>% 
  left_join(socio_eco_barriers_qog, by = c("country", "year", "iso")) %>% 
  left_join(welfare_regime_qog, by = c("country", "year", "iso"))







df <-
  df %>%
  group_by(country) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    # Creamos un grupo para contar los bloques consecutivos de stable.dem
    group_stable = with(rle(dem_backsliding == "stable.dem"),
                        rep(seq_along(values) * values, lengths)),
    
    # Contamos años consecutivos de stable.dem dentro del bloque
    years_stable = if_else(dem_backsliding == "stable.dem",
                           ave(seq_along(year), group_stable, FUN = seq_along),
                           0L),
    
    # Contador acumulado de todos los años de backsliding (aunque no sean consecutivos)
    years_backsliding = cumsum(dem_backsliding == "d.backsliding")
  ) %>%
  select(-group_stable, -years_stable) %>%
  ungroup() %>% 
    
    mutate(years_backsliding = ifelse(dem_backsliding == "d.backsliding", years_backsliding, 0)) 
    
    
names(df)

df_h2.1 %>% dplyr::select(merit_criteria_admin) %>% 
  ggplot(aes(x = merit_criteria_admin))+
  geom_histogram()

df_h2.1 %>% dplyr::select(attacks2) %>% 
  ggplot(aes(x = attacks2))+
  geom_histogram()