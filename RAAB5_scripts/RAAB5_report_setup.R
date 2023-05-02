# v1 12th Aug 2021

# Script to create new variables to support major data analyses and visualisations

# Categorise continuous age variable into 10-year binds in RAAB data file

age.groups.tens<-c("50-59","60-69","70-79","80+")
raab$age.groups.tens<-cut(raab$age,breaks=c(49,59,69,79,150),labels=age.groups.tens)

# Categorise continuous age variable into 10-year bins in population census file

popfives$age.groups.tens<-cut(popfives$ageStart,breaks=c(49,59,69,79,110),labels=age.groups.tens)

# Extract gender-specific subsets of census data

female.subpop<-popfives[popfives$gender=="female",]
male.subpop<-popfives[popfives$gender=="male",]

# Characterise study participant by visual acuity according to WHO thresholds
# Define numerators and denominators

raab$vi.denom <- case_when(raab$exam_status=="exam_status_examined" ~ 1, TRUE ~ 0)

#Bilateral VI cases by WHO categories using Peek Acuity logMAR vaules

raab <- raab %>% mutate(
  
  blind = case_when(raab$right_distance_acuity_presenting>=1.8 & raab$left_distance_acuity_presenting>=1.8 ~ 1, TRUE ~ 0), 
  severe.vi = case_when(pmin(raab$right_distance_acuity_presenting, raab$left_distance_acuity_presenting)==1.3 ~ 1, TRUE ~ 0),
  moderate.vi = case_when(pmin(raab$right_distance_acuity_presenting, raab$left_distance_acuity_presenting)==1.0 ~ 1, TRUE ~ 0),

)

raab <- raab %>% mutate(vi.levels = case_when(moderate.vi==1 ~ "moderate.vi", severe.vi==1 ~ "severe.vi", blind==1 ~ "blind"))

vi.levels<-c("blind","severe.vi","moderate.vi","mild.vi")

right.vi.levels<-c("right.blind","right.severe.vi","right.moderate.vi","right.mild.vi")
left.vi.levels<-c("left.blind","left.severe.vi","left.moderate.vi","left.mild.vi")
right.pinva.levels<-c("right.pinva.blind","right.pinva.severe.vi","right.pinva.moderate.vi","right.pinva.mild.vi")
left.pinva.levels<-c("left.pinva.blind","left.pinva.severe.vi","left.pinva.moderate.vi","left.pinva.mild.vi")
right.re.levels<-c("right.re.blind","right.re.severe.vi","right.re.moderate.vi","right.re.mild.vi")
left.re.levels<-c("left.re.blind","left.re.severe.vi","left.re.moderate.vi","left.re.mild.vi")
re.vi.levels<-c("re.blind","re.severe.vi","re.moderate.vi","re.mild.vi")
right.oc.levels<-c("right.poor.oc","right.borderline.oc","right.good.oc")
left.oc.levels<-c("left.poor.oc","left.borderline.oc","left.good.oc")
right.oc.pinva.levels<-c("right.poor.oc.pinva","right.borderline.oc.pinva","right.good.oc.pinva")
left.oc.pinva.levels<-c("left.poor.oc.pinva","left.borderline.oc.pinva","left.good.oc.pinva")
oc.levels<-c("poor.oc","borderline.oc","good.oc")

vi.tab<-as.data.frame(cbind(vi.levels,right.vi.levels,left.vi.levels,right.pinva.levels,left.pinva.levels,right.re.levels,left.re.levels,re.vi.levels))
oc.tab<-as.data.frame(cbind(left.oc.pinva.levels,right.oc.pinva.levels,right.oc.levels,left.oc.levels,oc.levels))
  
#List causes of blindness to ensure all get included, not just the ones identified in the survey.

raab.cause <- c("poor_vision_cause_uncorrected_refractive_error",   
                "poor_vision_cause_aphakia_uncorrected",             
                "poor_vision_cause_cataract_untreated",               
                "poor_vision_cause_cataract_surgical_complications",  
                "poor_vision_cause_trachomatous_corneal_opacity",     
                "poor_vision_cause_other_corneal_opacity",           
                "poor_vision_cause_phthisis",
                "poor_vision_cause_onchocerciasis",
                "poor_vision_cause_glaucoma", 
                "poor_vision_cause_diabetic_retinopathy",            
                "poor_vision_cause_age_related_macular_degeneration", 
                "poor_vision_cause_other_posterior_segment_disease",
                "poor_vision_cause_other_globe_or_cns_abnormalities")
				
#Define cumulative visual accuity counts
		
raab <- raab %>% mutate(
  
  moderate.cumulative = case_when(pmin(raab$right_distance_acuity_presenting,raab$left_distance_acuity_presenting)>=1.0 ~ 1, TRUE ~ 0),
  severe.cumulative = case_when(pmin(raab$right_distance_acuity_presenting, raab$left_distance_acuity_presenting)>=1.3 ~ 1, TRUE ~ 0),
  blind.cumulative = case_when(pmin(raab$right_distance_acuity_presenting,raab$left_distance_acuity_presenting)>=1.8 ~ 1, TRUE ~ 0)
  
)

raab <- raab %>% mutate(
  
  cumulative.vi = case_when(moderate.cumulative==1 ~ "moderate.cumulative", severe.cumulative==1 ~ "severe.cumulative", blind.cumulative==1 ~ "blind.cumulative")
  
)

cumulative.vi<-c("blind.cumulative","severe.cumulative","moderate.cumulative","mild.cumulative")

#Unilateral visual impairment

raab <- raab %>% mutate(
  
  blind.unilat = case_when((raab$right_distance_acuity_presenting>=1.8 & raab$left_distance_acuity_presenting==0.47)  |
                             (raab$left_distance_acuity_presenting>=1.8 & raab$right_distance_acuity_presenting==0.47) ~ 1, TRUE ~ 0),
  
  severe.unilat = case_when((raab$right_distance_acuity_presenting==1.3 & raab$left_distance_acuity_presenting==0.47)  |
                              (raab$left_distance_acuity_presentin==1.3 & raab$right_distance_acuity_presenting==0.47) ~ 1, TRUE ~ 0),
  
  moderate.unilat = case_when((raab$right_distance_acuity_presenting==1.0 & raab$left_distance_acuity_presenting==0.47)  |
                                (raab$left_distance_acuity_presenting==1.0 & raab$right_distance_acuity_presenting==0.47) ~ 1, TRUE ~ 0)
)


raab <- raab %>% mutate(
  
  unilat.vi = case_when(moderate.unilat==1 ~ "moderate.unilat", severe.unilat==1 ~ "severe.unilat", blind.unilat==1 ~ "blind.unilat")
  
)


unilat.vi<-c("blind.unilat","severe.unilat","moderate.unilat","mild.unilat")

#Cataract surgical outcome variables

raab <- raab %>% mutate(
  
  right_operable_618 = case_when((raab$lens_status_right=="lens_status_opacity" & raab$poor_vision_cause_right=="poor_vision_cause_cataract_untreated" & (raab$right_distance_acuity_pinhole>=1.0)) ~ 1, TRUE ~ 0),
  left_operable_618 = case_when((raab$lens_status_left=="lens_status_opacity" & raab$poor_vision_cause_left=="poor_vision_cause_cataract_untreated" & (raab$left_distance_acuity_pinhole>=1.0)) ~ 1, TRUE ~ 0),
  right_operable_660 = case_when((raab$lens_status_right=="lens_status_opacity" & raab$poor_vision_cause_right=="poor_vision_cause_cataract_untreated" & (raab$right_distance_acuity_pinhole>=1.3)) ~ 1, TRUE ~ 0),
  left_operable_660 = case_when((raab$lens_status_left=="lens_status_opacity" & raab$poor_vision_cause_left=="poor_vision_cause_cataract_untreated" & (raab$left_distance_acuity_pinhole>=1.3)) ~ 1, TRUE ~ 0),
  right_operable_360 = case_when((raab$lens_status_right=="lens_status_opacity" & raab$poor_vision_cause_right=="poor_vision_cause_cataract_untreated" & (raab$right_distance_acuity_pinhole>=1.8)) ~ 1, TRUE ~ 0),
  left_operable_360 = case_when((raab$lens_status_left=="lens_status_opacity" & raab$poor_vision_cause_left=="poor_vision_cause_cataract_untreated" & (raab$left_distance_acuity_pinhole>=1.8)) ~ 1, TRUE ~ 0),
 
#Operated definition 3: Excludes couched eyes and includes "no view of lens" if cataract surgical complications is recorded as cause of poor vision
  
  right_operated = case_when(((raab$lens_status_right=="lens_status_absent" & raab$surgery_type_right!="surgery_type_couching") | raab$lens_status_right=="lens_status_pseudophakia_no_pco" | raab$lens_status_right=="lens_status_pseudophakia_with_pco" | (raab$lens_status_right=="lens_status_no_view" & raab$poor_vision_cause_right=="poor_vision_cause_cataract_surgical_complications")) ~ 1, TRUE ~ 0),
  left_operated = case_when(((raab$lens_status_left=="lens_status_absent" & raab$surgery_type_left!="surgery_type_couching") | raab$lens_status_left=="lens_status_pseudophakia_no_pco" | raab$lens_status_left=="lens_status_pseudophakia_with_pco" | (raab$lens_status_left=="lens_status_no_view" & raab$poor_vision_cause_left=="poor_vision_cause_cataract_surgical_complications")) ~ 1, TRUE ~ 0)
)

# CSC and eCSC variables - new eCSC definition entered 16.06.22

raab <- raab %>% 
  
  mutate( 
    
#eCSC and CSC denominator definition: Any cause of VI in non-operated eyes (with addition of !=operated to define non-operated eye)

    x_case_618 = case_when(((raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.0 & raab$left_operated==1)  | (raab$right_operated==1 & raab$left_operated!=1 &raab$left_distance_acuity_pinhole>=1.0)) ~ 1, TRUE ~ 0),
    x_case_660 = case_when(((raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.3 & raab$left_operated==1)  | (raab$right_operated==1 & raab$left_operated!=1 &raab$left_distance_acuity_pinhole>=1.3)) ~ 1, TRUE ~ 0),
    x_case_360 = case_when(((raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.8 & raab$left_operated==1)  | (raab$right_operated==1 & raab$left_operated!=1 &raab$left_distance_acuity_pinhole>=1.8)) ~ 1, TRUE ~ 0),
    
    y_case_618 = case_when((raab$right_operated==1 & raab$left_operated==1) ~ 1, TRUE ~ 0),
    z_case_618 = case_when((raab$right_distance_acuity_pinhole>0.47 & raab$left_distance_acuity_pinhole>0.47) & (raab$right_operated!=1 & raab$left_operated!=1) & (raab$right_operable_618==1 | raab$left_operable_618==1) ~ 1, TRUE ~ 0),
    old_z_case_618 = case_when((raab$right_operable_618==1 & raab$left_operable_618==1) ~ 1, TRUE ~ 0),
    
    y_case_660 = case_when((raab$right_operated==1 & raab$left_operated==1) ~ 1, TRUE ~ 0),
    z_case_660 = case_when((raab$right_distance_acuity_pinhole>1 & raab$left_distance_acuity_pinhole>1) & (raab$right_operated!=1 & raab$left_operated!=1) & (raab$right_operable_660==1 | raab$left_operable_660==1) ~ 1, TRUE ~ 0),
    old_z_case_660 = case_when((raab$right_operable_660==1 & raab$left_operable_660==1) ~ 1, TRUE ~ 0),
    
    y_case_360 = case_when((raab$right_operated==1 & raab$left_operated==1) ~ 1, TRUE ~ 0),
    z_case_360 = case_when((raab$right_distance_acuity_pinhole>1.3 & raab$left_distance_acuity_pinhole>1.3) & (raab$right_operated!=1 & raab$left_operated!=1) & (raab$right_operable_360==1 | raab$left_operable_360==1) ~ 1, TRUE ~ 0),
    old_z_case_360 = case_when((raab$right_operable_360==1 & raab$left_operable_360==1) ~ 1, TRUE ~ 0),
    
#eCSC numerator definition: any cause of VI in non-operated eye

#NB syntax is a_case_[postopva]_[operablethresh]
    a_case_618_618 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=0.47) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.0)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=0.47) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.0)  ~ 1, TRUE ~ 0),
    a_case_618_660 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=0.47) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.3)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=0.47) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.3)  ~ 1, TRUE ~ 0),
    a_case_618_360 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=0.47) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.8)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=0.47) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.8)  ~ 1, TRUE ~ 0),

#    a_case_660_618 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=1.0) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.0)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=1.0) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.0)  ~ 1, TRUE ~ 0),
#    a_case_660_660 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=1.0) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.3)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=1.0) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.3)  ~ 1, TRUE ~ 0),
#    a_case_660_360 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=1.0) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.8)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=1.0) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.8)  ~ 1, TRUE ~ 0),

#    a_case_360_618 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=1.3) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.0)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=1.3) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.0)  ~ 1, TRUE ~ 0),
#    a_case_360_660 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=1.3) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.3)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=1.3) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.3)  ~ 1, TRUE ~ 0),
#    a_case_360_360 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=1.3) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.8)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=1.3) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.8)  ~ 1, TRUE ~ 0),

    b_case_618 = case_when((raab$right_operated==1 & raab$left_operated==1 & ifelse(as.numeric(raab$right_distance_acuity_presenting) < as.numeric(raab$left_distance_acuity_presenting),(raab$right_distance_acuity_presenting<1.0),(raab$left_distance_acuity_presenting<1.0))) ~ 1, TRUE ~ 0),
#    b_case_660 = case_when((raab$right_operated==1 & raab$left_operated==1 & pmin(raab$right_distance_acuity_presenting,raab$left_distance_acuity_presenting)<1.3) ~ 1, TRUE ~ 0),
#    b_case_360 = case_when((raab$right_operated==1 & raab$left_operated==1 & ifelse(as.numeric(raab$right_distance_acuity_presenting) < as.numeric(raab$left_distance_acuity_presenting),(raab$right_distance_acuity_presenting<1.8),(raab$left_distance_acuity_presenting<1.8))) ~ 1, TRUE ~ 0)
  )


# New vars for cataract VI at the person level (aligned with new eCSC definition denominator = unmet need for cataract surgery)
raab$cataract.blind <- (raab$z_case_360)+0
raab$cataract.severe.vi <- (raab$z_case_660)+0
raab$cataract.moderate.vi <- (raab$z_case_618)+0

# Vars for counting operated people (bilateral or unilateral, exclusive categories)
raab <- raab %>% mutate(
  bilat.operated = case_when(raab$right_operated==1 & raab$left_operated==1 ~1, TRUE~0)
)
raab <- raab %>% mutate(
  unilat.operated = case_when(bilat.operated!=1 & (raab$right_operated==1 | raab$left_operated==1) ~1, TRUE~0)
)

raab$total.operated<-(raab$bilat.operated==1 | raab$unilat.operated==1)+0

# Bilateral operable cataract cases used to report barriers to cataract surgery  
raab$bilateral_operable_cataract<-(raab$right_operable_660==1 & raab$left_operable_660)+0

raab <- raab %>% mutate(
  
  right.blind = case_when(raab$right_distance_acuity_presenting>=1.8 ~ 1, TRUE ~ 0), 
  
  right.severe.vi = case_when(raab$right_distance_acuity_presenting==1.3 ~ 1, TRUE ~ 0),
  
  right.moderate.vi = case_when(raab$right_distance_acuity_presenting==1.0 ~ 1, TRUE ~ 0),
  
  right.mild.vi = case_when(raab$right_distance_acuity_presenting==0.47 ~ 1, TRUE ~ 0),

  left.blind = case_when(raab$left_distance_acuity_presenting>=1.8 ~ 1, TRUE ~ 0), 
  
  left.severe.vi = case_when(raab$left_distance_acuity_presenting==1.3 ~ 1, TRUE ~ 0),
  
  left.moderate.vi = case_when(raab$left_distance_acuity_presenting==1.0 ~ 1, TRUE ~ 0),
  
  left.mild.vi = case_when(raab$left_distance_acuity_presenting==0.47 ~ 1, TRUE ~ 0)

)


raab <- raab %>% mutate(
  
  right.pinva.blind = case_when(raab$right_distance_acuity_pinhole>=1.8 ~ 1, TRUE ~ 0), 
  
  right.pinva.severe.vi = case_when(raab$right_distance_acuity_pinhole==1.3 ~ 1, TRUE ~ 0),
  
  right.pinva.moderate.vi = case_when(raab$right_distance_acuity_pinhole==1.0 ~ 1, TRUE ~ 0),
  
  right.pinva.mild.vi = case_when(raab$right_distance_acuity_pinhole==0.47 ~ 1, TRUE ~ 0),
  
  left.pinva.blind = case_when(raab$left_distance_acuity_pinhole>=1.8 ~ 1, TRUE ~ 0), 
  
  left.pinva.severe.vi = case_when(raab$left_distance_acuity_pinhole==1.3 ~ 1, TRUE ~ 0),
  
  left.pinva.moderate.vi = case_when(raab$left_distance_acuity_pinhole==1.0 ~ 1, TRUE ~ 0),
  
  left.pinva.mild.vi = case_when(raab$left_distance_acuity_pinhole==0.47 ~ 1, TRUE ~ 0)

)

raab <- raab %>% mutate(
  
  right.pinva.levels = case_when(right.pinva.mild.vi==1 ~ "right.pinva.mild.vi", right.pinva.moderate.vi==1 ~ "right.pinva.moderate.vi", right.pinva.severe.vi==1 ~ "right.pinva.severe.vi", right.pinva.blind==1 ~ "right.pinva.blind")
  
)

raab <- raab %>% mutate(
  
  left.pinva.levels = case_when(left.pinva.mild.vi==1 ~ "left.pinva.mild.vi", left.pinva.moderate.vi==1 ~ "left.pinva.moderate.vi", left.pinva.severe.vi==1 ~ "left.pinva.severe.vi", left.pinva.blind==1 ~ "left.pinva.blind")
  
)



raab <- raab %>% mutate(
  
  right.vi.levels = case_when(right.mild.vi==1 ~ "right.mild.vi", right.moderate.vi==1 ~ "right.moderate.vi", right.severe.vi==1 ~ "right.severe.vi", right.blind==1 ~ "right.blind"),
  left.vi.levels = case_when(left.mild.vi==1 ~ "left.mild.vi", left.moderate.vi==1 ~ "left.moderate.vi", left.severe.vi==1 ~ "left.severe.vi", left.blind==1 ~ "left.blind")
  
)

raab <- raab %>% mutate(
  
  right.poor.oc = case_when(raab$right_distance_acuity_presenting>1.0 ~ 1, TRUE ~ 0), 
  
  right.borderline.oc = case_when(raab$right_distance_acuity_presenting==1.0 ~ 1, TRUE ~ 0),
  
  right.good.oc = case_when(raab$right_distance_acuity_presenting==0.47 ~ 1, TRUE ~ 0),
  
  left.poor.oc = case_when(raab$left_distance_acuity_presenting>1.0 ~ 1, TRUE ~ 0), 
  
  left.borderline.oc = case_when(raab$left_distance_acuity_presenting==1.0 ~ 1, TRUE ~ 0),
  
  left.good.oc = case_when(raab$left_distance_acuity_presenting==0.47 ~ 1, TRUE ~ 0),
  
)

raab <- raab %>% mutate(
  
  right.oc.levels = case_when(right.good.oc==1 ~ "right.good.oc", right.borderline.oc==1 ~ "right.borderline.oc", right.poor.oc==1 ~ "right.poor.oc"),
  left.oc.levels = case_when(left.good.oc==1 ~ "left.good.oc", left.borderline.oc==1 ~ "left.borderline.oc", left.poor.oc==1 ~ "left.poor.oc")
  
)

raab <- raab %>% mutate(
  
  right.poor.oc.pinva = case_when(raab$right_distance_acuity_pinhole>1.0 ~ 1, TRUE ~ 0), 
  
  right.borderline.oc.pinva = case_when(raab$right_distance_acuity_pinhole==1.0 ~ 1, TRUE ~ 0),
  
  right.good.oc.pinva = case_when(raab$right_distance_acuity_pinhole==0.47 ~ 1, TRUE ~ 0),
  
  left.poor.oc.pinva = case_when(raab$left_distance_acuity_pinhole>1.0 ~ 1, TRUE ~ 0), 
  
  left.borderline.oc.pinva = case_when(raab$left_distance_acuity_pinhole==1.0 ~ 1, TRUE ~ 0),
  
  left.good.oc.pinva = case_when(raab$left_distance_acuity_pinhole==0.47 ~ 1, TRUE ~ 0),
  
)

raab <- raab %>% mutate(
  
  right.oc.pinva.levels = case_when(right.good.oc.pinva==1 ~ "right.good.oc.pinva", right.borderline.oc.pinva==1 ~ "right.borderline.oc.pinva", right.poor.oc.pinva==1 ~ "right.poor.oc.pinva"),
  left.oc.pinva.levels = case_when(left.good.oc.pinva==1 ~ "left.good.oc.pinva", left.borderline.oc.pinva==1 ~ "left.borderline.oc.pinva", left.poor.oc.pinva==1 ~ "left.poor.oc.pinva")
  
)

#Surgery places

surgery_places <- c("surgery_place_camp_improvised","surgery_place_gov_hospital","surgery_place_private_hospital","surgery_place_traditional","surgery_place_voluntary_hospital")

#cataract surgery types and outcomes

raab <- raab %>% mutate(
  
  right.operated.eyes.denom = case_when((lens_status_right=="lens_status_absent" | lens_status_right=="lens_status_pseudophakia_no_pco" | lens_status_right=="lens_status_pseudophakia_with_pco" | (raab$lens_status_right=="lens_status_no_view" & raab$poor_vision_cause_right=="poor_vision_cause_cataract_surgical_complications")) ~ 1, TRUE ~0),
  left.operated.eyes.denom = case_when((lens_status_left=="lens_status_absent" | lens_status_left=="lens_status_pseudophakia_no_pco" | lens_status_left=="lens_status_pseudophakia_with_pco" | (raab$lens_status_left=="lens_status_no_view" & raab$poor_vision_cause_left=="poor_vision_cause_cataract_surgical_complications")) ~ 1, TRUE ~0)
  
)

raab <- raab %>% mutate(
  
  right.re.blind = case_when(raab$poor_vision_cause_right=="poor_vision_cause_uncorrected_refractive_error" & raab$right_distance_acuity_presenting>=1.8 ~ 1, TRUE ~ 0), 
  
  right.re.severe.vi = case_when(raab$poor_vision_cause_right=="poor_vision_cause_uncorrected_refractive_error" & raab$right_distance_acuity_presenting==1.3 ~ 1, TRUE ~ 0),
  
  right.re.moderate.vi = case_when(raab$poor_vision_cause_right=="poor_vision_cause_uncorrected_refractive_error" & raab$right_distance_acuity_presenting==1.0 ~ 1, TRUE ~ 0),
  
  left.re.blind = case_when(raab$poor_vision_cause_left=="poor_vision_cause_uncorrected_refractive_error" & raab$left_distance_acuity_presenting>=1.8 ~ 1, TRUE ~ 0), 
  
  left.re.severe.vi = case_when(raab$poor_vision_cause_left=="poor_vision_cause_uncorrected_refractive_error" & raab$left_distance_acuity_presenting==1.3 ~ 1, TRUE ~ 0),
  
  left.re.moderate.vi = case_when(raab$poor_vision_cause_left=="poor_vision_cause_uncorrected_refractive_error" & raab$left_distance_acuity_presenting==1.0 ~ 1, TRUE ~ 0)
)

raab <- raab %>% mutate(
  
  re.blind = case_when(raab$poor_vision_cause_right=="poor_vision_cause_uncorrected_refractive_error" & (raab$right_distance_acuity_presenting>=1.8 | raab$left_distance_acuity_presenting>=1.8) ~ 1, TRUE ~ 0), 
  
  re.severe.vi = case_when(raab$poor_vision_cause_right=="poor_vision_cause_uncorrected_refractive_error" & ( (raab$right_distance_acuity_presenting==1.3 & raab$left_distance_acuity_presenting>=1.3) | (raab$left_distance_acuity_presenting==1.3 & raab$right_distance_acuity_presenting>=1.3) ) ~ 1, TRUE ~ 0),
  
  re.moderate.vi = case_when(raab$poor_vision_cause_right=="poor_vision_cause_uncorrected_refractive_error" & ( (raab$right_distance_acuity_presenting==1.0 & raab$left_distance_acuity_presenting>=1.0) | (raab$left_distance_acuity_presenting==1.0 & raab$right_distance_acuity_presenting>=1.0) ) ~ 1, TRUE ~ 0)
)


raab <- raab %>% mutate(
  
  re.vi.levels = case_when(re.moderate.vi==1 ~ "re.moderate.vi", re.severe.vi==1 ~ "re.severe.vi", re.blind==1 ~ "re.blind")

)

#eREC/REC variables - note output not included in RAAB report as no 6/12 data available

raab <- raab %>% mutate(
  
  better.eye.pva = pmin(raab$right_distance_acuity_presenting, raab$left_distance_acuity_presenting),
  
  better.eye.pinva = pmin(raab$right_distance_acuity_pinhole, raab$left_distance_acuity_pinhole)
  
)

if(!is.logical(raab$spectacles_used_distance)){
  
    raab$spectacles_used_distance<-as.logical(raab$spectacles_used_distance)

  }else{
    
    raab$spectacles_used_distance<-raab$spectacles_used_distance
    
  }

if(!is.logical(raab$spectacles_used_near)){
  
  raab$spectacles_used_near<-as.logical(raab$spectacles_used_near)
  
}else{
  
  raab$spectacles_used_near<-raab$spectacles_used_near
  
}


raab <- raab %>% mutate(
  
  aa_case = case_when(raab$spectacles_used_distance==TRUE & raab$better.eye.pva==0.47 ~ 1, TRUE ~ 0)
  
)

raab <- raab %>% mutate(
  
  bb_case = case_when(raab$aa_case==0 & raab$spectacles_used_distance==TRUE & raab$better.eye.pva>0.47 & raab$better.eye.pinva==0.47 ~ 1, TRUE ~ 0),
  cc_case = case_when(raab$spectacles_used_distance==FALSE & raab$better.eye.pva>0.47 & raab$better.eye.pinva==0.47 ~ 1, TRUE ~ 0)
  
)

