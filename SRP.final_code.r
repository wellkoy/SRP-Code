# SRP Project
# Author: Catherine Wellborn Koy (Emma [ocs])[robert-volumes]
# 0--0--0--0--0--0--0--0--0--0//

# 0--0--0--0--0//
# Libraries
# 0--0--0--0--0//
library(dplyr)
library(tidyverse)
library(readxl)
library(rempsyc)

# 0--0--0--0--0//
# Data Paths
# 0--0--0--0--0//

data_path  <- "~/OneDrive - The University of Texas at Dallas/Damme, Katherine Steffen's files - R21MH136408_PLEs_Menarche/ABCD_6.0/Data"
gen_path   <- file.path(data_path, "abcd_general")
mh_path    <- file.path(data_path, "Mental_Health")
img_path   <- file.path(data_path, "Imaging", "Structural_MRI")
oc_path    <- "~/OneDrive - The University of Texas at Dallas/Damme, Katherine Steffen's files - R21MH136408_PLEs_Menarche/AdditionalABCDMeasures/ABCD Prenatal Complications/ABCD_6.0/Outputs"
aces_path  <- "~/OneDrive - The University of Texas at Dallas/Damme, Katherine Steffen's files - R21MH136408_PLEs_Menarche/AdditionalABCDMeasures/ABCD Adverse Childhood Experiences/ABCD_5.0/Outputs"
amyg_path  <- "~/OneDrive - The University of Texas at Dallas/Damme, Katherine Steffen's files - R21MH136408_PLEs_Menarche/AdditionalABCDMeasures/ABCD Amygdala Subvolumes"

# 0--0--0--0--0//
# General Data
# 0--0--0--0--0//

gen_path <- file.path(data_path,"abcd_general")

# Reading in Visit/Demographic tsv files
SRP_Data <- read_tsv(paste0(gen_path, '/ab_g_dyn.tsv'))
SRP_Data <-  SRP_Data %>%
  select(participant_id, 
         session_id, 
         ab_g_dyn__visit_age, # Participant age @ each session
         ab_g_dyn__design_mr__serial) # Scanner Serial #
        #ab_g_dyn__design_mr__model) # Scanner Model
age <- SRP_Data %>%
  select(participant_id, 
         session_id, 
         ab_g_dyn__visit_age)
SRP_Data <- filter(SRP_Data, session_id == "ses-00A") # Filtering for only Baseline data
SRP_Data <- SRP_Data %>%
  rename(age = ab_g_dyn__visit_age,
         scanner_id = ab_g_dyn__design_mr__serial)
#scanner_model = ab_g_dyn__design_mr__model)

# Reading in Sex/Race tsv files
stc <- read_tsv(paste0(gen_path, '/ab_g_stc.tsv'))
stc <- stc %>%
  select(participant_id, 
         ab_g_stc__cohort_sex, # Participant sex
         ab_g_stc__cohort_ethnrace__leg, # Participant ethnicity 
         ab_g_stc__design_famrel) # Family ID 
SRP_Data <-  merge(SRP_Data, stc, by = "participant_id")

SRP_Data <- SRP_Data %>%
  rename(sex = ab_g_stc__cohort_sex,
         ethnrace = ab_g_stc__cohort_ethnrace__leg,
         family_id = ab_g_stc__design_famrel)
SRP_Data <- SRP_Data %>%
  mutate(sex = recode(sex,
                      '1' = 'Male',
                      '2' = 'Female'))
SRP_Data <- SRP_Data %>%
  mutate(ethnrace = recode(ethnrace, 
                           '1'= 'Hispanic',
                           '2' = 'White',
                           '3' = 'Black',
                           '4' = 'Asian',
                           '13' = 'Other'))
demo <- read_tsv(paste0(gen_path, '/ab_p_demo.tsv'))
demo <- demo %>%
  select(participant_id,
         session_id,
         ab_p_demo__income__hhold_001) # Total combined family income for the past 12 months
SRP_Data <- merge(SRP_Data, demo, by = c("participant_id", "session_id"), all.x = TRUE)
SRP_Data <- SRP_Data %>%
  rename(fam_income = ab_p_demo__income__hhold_001)
SRP_Data <- SRP_Data %>%
  mutate(fam_income = na_if(fam_income, 'n/a'),
         fam_income = na_if(fam_income, '999'),
         fam_income = na_if(fam_income, '777'))
SRP_Data <- SRP_Data %>%
  mutate(fam_income = factor(SRP_Data$fam_income, 
                             levels = c("1","2","3","4","5","6","7","8","9","10"), 
                             labels = c("Less than $5,000",
                                        "$5,000 through $11,999",
                                        "$12,000 through $15,999", 
                                        "$16,000 through $24,999", 
                                        "$25,000 through $34,999", 
                                        "$35,000 through $49,999", 
                                        "$50,000 through $74,999", 
                                        "$75,000 through $99,999", 
                                        "$100,000 through $199,999", 
                                        "$200,000 and greater"), ordered=TRUE))


# 0--0--0--0//
# OCs Data
# 0--0--0--0//

OCs <- read_csv(file.path(oc_path, "Obstetric_Complications_9_12_25.csv"))
OCs <- OCs %>%
  select(participant_id,
         C_Section_Y1N0, Bleeding_Preg_Y1N0, Pre_Eclampsia_Y1N0,
         Blue_at_Birth_Y1N0, Required_Resusc_Y1N0, Neonatal_Apnea_Y1N0,
         Planned_Preg_Y1N0, UTI_Y1N0, Rubella_Y1N0,
         Relevant_Med_1_Y1N0, Relevant_Med_2_Y1N0, Relevant_Med_3_Y1N0,
         Relevant_Med_4_Y1N0, Relevant_Med_5_Y1N0, Relevant_Med_6_Y1N0,
         Relevant_Med_7_Y1N0, Relevant_Med_8_Y1N0,
         All_Relevant_Med_Count, Hypoxia_Flag, Infection_Flag, Stress_Flag)

SRP_Data <- merge(SRP_Data, OCs, by = "participant_id", all.x = TRUE)

# "Don't Know" (999) to NA
SRP_Data <- SRP_Data %>%
  mutate(across(c(C_Section_Y1N0, Bleeding_Preg_Y1N0, Blue_at_Birth_Y1N0,
                  Required_Resusc_Y1N0, Neonatal_Apnea_Y1N0, Planned_Preg_Y1N0,
                  UTI_Y1N0, Rubella_Y1N0),
                ~ na_if(., 999)))

# Exclude twins
#SRP_Data <- SRP_Data[SRP_Data$ab_g_stc__design_famrel < 2, ]
#SRP_Data <- SRP_Data %>% select(-ab_g_stc__design_famrel)

# Exclude twins
SRP_Data <- SRP_Data[SRP_Data$family_id < 2, ]
SRP_Data <- SRP_Data %>% select(-family_id)

# OC total count
SRP_Data <- SRP_Data %>%
  mutate(OC_Total = rowSums(select(., C_Section_Y1N0, Bleeding_Preg_Y1N0,
                                   Pre_Eclampsia_Y1N0, Blue_at_Birth_Y1N0,
                                   Required_Resusc_Y1N0, Neonatal_Apnea_Y1N0,
                                   Stress_Flag, UTI_Y1N0, Rubella_Y1N0,
                                   All_Relevant_Med_Count),
                            na.rm = TRUE))

# 0--0--0--0//
# ACEs Data
# 0--0--0--0//

ACEs_data <- read_csv(file.path(aces_path, "ACES_baseline_output.csv"))

ACEs_clean <- ACEs_data %>%
  select(src_subject_id,
         rel_family_id,
         demo_comb_income_hrcode,           # SES
         ACEs_Total,                        # Total ACEs
         # -- Abuse --
         Abuse_ACEs,                        # Total Abuse ACEs
         SexualAbuse1Y0N,
         EmotionalAbuse1Y0N,
         PhysicalAbuse1Y0N,
         FamilyEnviron_Parent_Violence1Y0N,
         FamilyEnviron_Youth_Violence1Y0N,
         # -- Neglect --
         Neglect_ACEs,                      # Total Neglect ACEs
         ParentalCriminal1Y0N,
         ParentalMDD1Y0N,
         ParentalSuicide1Y0N,
         ParentalAlcoholProblems1Y0N,
         ParentalEmotionalNeglectProxy,
         HouseholdMentalIllness,
         PhysicalNeglect,
         HouseholdViolence,
         WitnessViolenceHousehold) %>%
  mutate(src_subject_id = str_replace(src_subject_id, "NDAR_INV", "sub-")) %>%
  rename(participant_id = src_subject_id)

# 0--0--0--0--0--0//
# Amygdala Data
# 0--0--0--0--0--0//

amygdala_vol_data <- read.csv(file.path(amyg_path, "All_Amyg_Volumes.csv")) #new volumes
amygdala_vol_data <- amygdala_vol_data %>%
  select(participant_id, session_id,
         lh_Lateral.nucleus, lh_Basal.nucleus, lh_Accessory.Basal.nucleus,
         lh_Anterior.amygdaloid.area.AAA, lh_Central.nucleus, lh_Medial.nucleus,
         lh_Cortical.nucleus, lh_Corticoamygdaloid.transition, lh_Paralaminar.nucleus,
         lh_Whole_amygdala,
         rh_Lateral.nucleus, rh_Basal.nucleus, rh_Accessory.Basal.nucleus,
         rh_Anterior.amygdaloid.area.AAA, rh_Central.nucleus, rh_Medial.nucleus,
         rh_Cortical.nucleus, rh_Corticoamygdaloid.transition, rh_Paralaminar.nucleus,
         rh_Whole_amygdala)

amygdala_vol_data <- amygdala_vol_data %>%
  mutate(
    bilateral_lateral     = lh_Lateral.nucleus     + rh_Lateral.nucleus,
    bilateral_basal       = lh_Basal.nucleus        + rh_Basal.nucleus,
    bilateral_accbasal    = lh_Accessory.Basal.nucleus + rh_Accessory.Basal.nucleus,
    bilateral_AAA         = lh_Anterior.amygdaloid.area.AAA + rh_Anterior.amygdaloid.area.AAA,
    bilateral_central     = lh_Central.nucleus      + rh_Central.nucleus,
    bilateral_medial      = lh_Medial.nucleus       + rh_Medial.nucleus,
    bilateral_cortical    = lh_Cortical.nucleus     + rh_Cortical.nucleus,
    bilateral_cortitrans  = lh_Corticoamygdaloid.transition + rh_Corticoamygdaloid.transition,
    bilateral_paralaminar = lh_Paralaminar.nucleus  + rh_Paralaminar.nucleus,
    BLA = bilateral_lateral + bilateral_basal + bilateral_accbasal + bilateral_paralaminar,
    CM  = bilateral_central + bilateral_medial,
    SF  = bilateral_cortitrans + bilateral_cortical + bilateral_AAA
  )

# 0--0--0--0--0--0--0--0--0--0--0//
# Whole Brain Volume (covariate)
# 0--0--0--0--0--0--0--0--0--0--0//

whole_brain_data <- read_tsv(file.path(img_path, "mr_y_smri__vol__aseg.tsv")) %>%
  select(participant_id, session_id, whole_brain_vol = mr_y_smri__vol__aseg__whb_sum)

# 0--0--0--0--0--0--0--0--0--0--0//
# PDS Data (Puberty)
# 0--0--0--0--0--0--0--0--0--0--0//

pds_data <- read_tsv(file.path(data_path, "Physical_Health", "ph_y_pds.tsv")) %>%
  select(participant_id, session_id, ph_y_pds__f_categ, ph_y_pds__m_categ)
# Merge PDS into SRP_Data
SRP_Data <- merge(SRP_Data, pds_data, by = c("participant_id", "session_id"), all.x = TRUE)

# Create pds_fem and pds_male columns
SRP_Data <- SRP_Data %>%
  mutate(ph_y_pds__m_categ = na_if(ph_y_pds__m_categ, "n/a"),
         ph_y_pds__m_categ = na_if(ph_y_pds__m_categ, "999"),
         ph_y_pds__m_categ = na_if(ph_y_pds__m_categ, "777"),
         ph_y_pds__f_categ = na_if(ph_y_pds__f_categ, "n/a"),
         ph_y_pds__f_categ = na_if(ph_y_pds__f_categ, "999"),
         ph_y_pds__f_categ = na_if(ph_y_pds__f_categ, "777"),
         pds_fem  = ph_y_pds__f_categ,
         pds_male = ph_y_pds__m_categ)

# Convert to ordered factors
SRP_Data <- SRP_Data %>%
  mutate(pds_fem = factor(pds_fem,
                          levels = c("1","2","3","4","5"),
                          labels = c("Pre pubertal",
                                     "Early pubertal",
                                     "Mid pubertal", 
                                     "Late pubertal", 
                                     "Post pubertal"), ordered = TRUE),
         pds_male = factor(pds_male,
                           levels = c("1","2","3","4","5"),
                           labels = c("Pre pubertal",
                                      "Early pubertal",
                                      "Mid pubertal",
                                      "Late pubertal",
                                      "Post pubertal"), ordered = TRUE))
# 0--0--0--0--0--0--0--0--0--0--0//
# CBCL Data (Psychopathology)
# 0--0--0--0--0--0--0--0--0--0--0//

# Load CBCL file and select only the subscale sum scores we need
cbcl_data <- read_tsv(file.path(mh_path,  "mh_p_cbcl.tsv")) %>%
  select(participant_id, session_id,
         mh_p_cbcl__synd__int_sum,       # Internalizing total
         mh_p_cbcl__synd__anxdep_sum,    # Anxious/Depressed subscale
         mh_p_cbcl__synd__wthdep_sum,    # Withdrawn/Depressed subscale
         mh_p_cbcl__synd__ext_sum,       # Externalizing total
         mh_p_cbcl__synd__attn_sum,      # Attention problems subscale
         mh_p_cbcl__synd__aggr_sum,      # Aggressive behavior subscale
         mh_p_cbcl__synd__rule_sum,      # Rule breaking subscale
         mh_p_cbcl__dsm__anx_sum,        # DSM anxiety (separate)
         mh_p_cbcl__dsm__dep_sum)        # DSM depression (separate))   

# 0--0--0--0--0--0--0--0--0--0--0//
# Final Merge & Variable Creation
# 0--0--0--0--0--0--0--0--0--0--0//
final_data <- SRP_Data %>%
  left_join(amygdala_vol_data, by = c("participant_id", "session_id")) %>%
  left_join(ACEs_clean,        by = "participant_id") %>%
  left_join(whole_brain_data,  by = c("participant_id", "session_id")) %>%
  left_join(cbcl_data,         by = c("participant_id", "session_id"))

final_data <- final_data %>%
  mutate(
    whole_Amyg_vol    = lh_Whole_amygdala + rh_Whole_amygdala,
    cumulative_stress = OC_Total + ACEs_Total,
    demographics      = factor(ethnrace, levels = c("White", "Black", "Hispanic", "Asian", "Other")),
    pds_score = as.numeric(ifelse(sex == "Male",
                                  ph_y_pds__m_categ,
                                  ph_y_pds__f_categ)),
    internalizing_z = as.numeric(scale(mh_p_cbcl__synd__int_sum)),
    anxdep_z        = as.numeric(scale(mh_p_cbcl__synd__anxdep_sum)),
    wthdep_z        = as.numeric(scale(mh_p_cbcl__synd__wthdep_sum)),
    externalizing_z = as.numeric(scale(mh_p_cbcl__synd__ext_sum)),
    attn_z          = as.numeric(scale(mh_p_cbcl__synd__attn_sum)),
    aggr_z          = as.numeric(scale(mh_p_cbcl__synd__aggr_sum)),
    rule_z          = as.numeric(scale(mh_p_cbcl__synd__rule_sum)),
    anx_z           = as.numeric(scale(mh_p_cbcl__dsm__anx_sum)),
    dep_z           = as.numeric(scale(mh_p_cbcl__dsm__dep_sum))
  )

# 0--0--0--0--0--0--0--0--0--0--0//
# Filter to Baseline Session Only
# 0--0--0--0--0--0--0--0--0--0--0//

baseline_data <- final_data %>% filter(session_id == "ses-00A")

# 0--0--0--0--0--0--0--0--0--0--0--0//
# AIM 1: Does early life stress predict INTERNALIZING psychopathology?
# 0--0--0--0--0--0--0--0--0--0--0--0//

internalizing_stress_model <- lm(internalizing_z ~ OC_Total + Abuse_ACEs + Neglect_ACEs +
                                   age + sex + demographics +
                                   fam_income + whole_brain_vol + pds_score,
                                 data = baseline_data)
summary(internalizing_stress_model)

#anxdep_stress_model <- lm(anxdep_z ~ OC_Total + Abuse_ACEs + Neglect_ACEs +
#                            age + sex + demographics +
#                            fam_income + whole_brain_vol + pds_score,
 #                         data = baseline_data)
#summary(anxdep_stress_model)

#wthdep_stress_model <- lm(wthdep_z ~ OC_Total + Abuse_ACEs + Neglect_ACEs +
#                            age + sex + demographics +
 #                           fam_income + whole_brain_vol + pds_score,
 #                         data = baseline_data)
#summary(wthdep_stress_model)

# 0--0--0--0--0--0--0--0--0--0--0--0//
# AIM 2: Does early life stress predict EXTERNALIZING psychopathology?
# 0--0--0--0--0--0--0--0--0--0--0--0//

externalizing_stress_model <- lm(externalizing_z ~ OC_Total + Abuse_ACEs + Neglect_ACEs +
                                   age + sex + demographics + whole_brain_vol +
                                   fam_income + pds_score,
                                 data = baseline_data)
summary(externalizing_stress_model)

# 0--0--0--0--0--0--0--0--0--0--0--0//
# AIM 3: Does whole amygdala volume predict psychopathology?
# 0--0--0--0--0--0--0--0--0--0--0--0//
# Internalizing moderation

int_vol_model <- lm(internalizing_z ~ whole_Amyg_vol + OC_Total + Abuse_ACEs + Neglect_ACEs + whole_brain_vol +
                      age + sex + demographics +
                      fam_income + pds_score,
                    data = baseline_data)
summary(int_vol_model)

ext_vol_model <- lm(externalizing_z ~ whole_Amyg_vol + OC_Total + Abuse_ACEs + Neglect_ACEs + whole_brain_vol +
                      age + sex + demographics +
                      fam_income + pds_score,
                    data = baseline_data)
summary(ext_vol_model)











#first tries
#int_ernalizing_amyg_mod <- lm(internalizing_z ~ whole_Amyg_vol * OC_Total + Neglect_ACEs + Abuse_ACEs +
  #                              age + sex + demographics +
   #                             fam_income + whole_brain_vol + pds_score,
       #                       data = baseline_data)
#summary(int_ernalizing_amyg_mod)

#internalizing_amyg_mod <- lm(internalizing_z ~ OC_Total + ACEs_Total * whole_Amyg_vol +
   #                            age + sex + demographics +
  #                             fam_income + pds_score,
  #                           data = baseline_data)
#summary(internalizing_amyg_mod)

#second improved example
#int_ernalizing_amyg_mod <- lm(internalizing_z ~ OC_Total * whole_Amyg_vol + Neglect_ACEs * whole_Amyg_vol + Abuse_ACEs * whole_Amyg_vol +
  #                             age + sex + demographics +
  #                            fam_income + whole_brain_vol + pds_score,
  #                           data = baseline_data)
#summary(int_ernalizing_amyg_mod)

#suggestion from emma
#int_ernalizing_amyg_mod <- lm(internalizing_z ~ OC_Total * whole_Amyg_vol * Neglect_ACEs * whole_Amyg_vol * Abuse_ACEs * whole_Amyg_vol +
 #                               age + sex + demographics +
 #                               fam_income + whole_brain_vol + pds_score,
  #                            data = baseline_data)
#summary(int_ernalizing_amyg_mod)





# Externalizing moderation
#externalizing_amyg_mod <- lm(externalizing_z ~ OC_Total * whole_Amyg_vol + Neglect_ACEs * whole_Amyg_vol + Abuse_ACEs * whole_Amyg_vol +
 #                              age + sex + demographics +
   #                            fam_income + whole_brain_vol + pds_score,
 #                            data = baseline_data)
#summary(externalizing_amyg_mod)

# 0--0--0--0--0--0--0--0--0--0--0--0//
# Confirmation on Lit model (Dr. D suggested)
# Based on literature: depression = smaller amygdala, anxiety = larger amygdala
# model to test interaction between anxiety and depression on amygdala volume
# 0--0--0--0--0--0--0--0--0--0--0--0//

#maybe_model <- lm(whole_Amyg_vol ~ anx_z * dep_z +
       #                    attn_z + aggr_z + rule_z +
     #                      OC_Total + Abuse_ACEs + Neglect_ACEs +
        #                   age + sex + demographics +
  #                         fam_income + whole_brain_vol + pds_score,
#                         data = baseline_data)
#summary(maybe_model)
 
###############################
#relation_mod <- lm(whole_Amyg_vol ~ OC_Total + Abuse_ACEs + Neglect_ACEs + whole_brain_vol +
 #                    age + sex + demographics +
   #                  fam_income + pds_score,
   #                data = baseline_data)
#summary(relation_mod)

##############################



