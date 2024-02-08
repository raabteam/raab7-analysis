# v1 12th Aug 2021

# v2 17.01.22 IM
# v3 30.06.22 IM - ADDITION OF NEW ECSC TERMS, NEW WGQ VARIABLES, NEW DR VARIABLES
# v4 09.08.22 IM - added myopic degeneration to list of VI causes
# v5 18.08.22 IM - fix to new eCSC calculation x and a case defs
# v6 13.12.22 IM - update to definition of cataract operated eye
# v7 18.09.23 IM - incorporate new lens status options for aphakia

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

#Bilateral VI cases by WHO categories using Peek Acuity logMAR values

raab <- raab %>% mutate(
  
  blind = case_when(raab$right_distance_acuity_presenting>=1.8 & raab$left_distance_acuity_presenting>=1.8 ~ 1, TRUE ~ 0), 
  severe.vi = case_when(pmin(raab$right_distance_acuity_presenting, raab$left_distance_acuity_presenting)==1.3 ~ 1, TRUE ~ 0),
  moderate.vi = case_when(pmin(raab$right_distance_acuity_presenting, raab$left_distance_acuity_presenting)==1.0 ~ 1, TRUE ~ 0),
  mild.vi = case_when(pmin(raab$right_distance_acuity_presenting, raab$left_distance_acuity_presenting)==0.47 ~ 1, TRUE ~ 0)
  
)

raab <- raab %>% mutate(vi.levels = case_when(mild.vi==1 ~ "mild.vi", moderate.vi==1 ~ "moderate.vi", severe.vi==1 ~ "severe.vi", blind==1 ~ "blind"))

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
                "poor_vision_cause_pterygium",
                "poor_vision_cause_phthisis",
                "poor_vision_cause_onchocerciasis",
                "poor_vision_cause_glaucoma", 
                "poor_vision_cause_diabetic_retinopathy",            
                "poor_vision_cause_age_related_macular_degeneration", 
                "poor_vision_cause_other_posterior_segment_disease",
                "poor_vision_cause_myopic_degeneration",
                "poor_vision_cause_other_globe_or_cns_abnormalities")
				
#Define cumulative visual acuity counts
		
raab <- raab %>% mutate(
  
  mild.cumulative = case_when(pmin(raab$right_distance_acuity_presenting,raab$left_distance_acuity_presenting)>=0.47 ~ 1, TRUE ~ 0),
  moderate.cumulative = case_when(pmin(raab$right_distance_acuity_presenting,raab$left_distance_acuity_presenting)>=1.0 ~ 1, TRUE ~ 0),
  severe.cumulative = case_when(pmin(raab$right_distance_acuity_presenting, raab$left_distance_acuity_presenting)>=1.3 ~ 1, TRUE ~ 0),
  blind.cumulative = case_when(pmin(raab$right_distance_acuity_presenting,raab$left_distance_acuity_presenting)>=1.8 ~ 1, TRUE ~ 0)
  
)

raab <- raab %>% mutate(
  
  cumulative.vi = case_when(mild.cumulative==1 ~ "mild.cumulative", moderate.cumulative==1 ~ "moderate.cumulative", severe.cumulative==1 ~ "severe.cumulative", blind.cumulative==1 ~ "blind.cumulative")
  
)

cumulative.vi<-c("blind.cumulative","severe.cumulative","moderate.cumulative","mild.cumulative")


#Unilateral vision impairment

raab <- raab %>% mutate(
  
  blind.unilat = case_when((raab$right_distance_acuity_presenting>=1.8 & raab$left_distance_acuity_presenting==0.3)  |
                             (raab$left_distance_acuity_presenting>=1.8 & raab$right_distance_acuity_presenting==0.3) ~ 1, TRUE ~ 0),
  
  severe.unilat = case_when((raab$right_distance_acuity_presenting==1.3 & raab$left_distance_acuity_presenting==0.3)  |
                              (raab$left_distance_acuity_presentin==1.3 & raab$right_distance_acuity_presenting==0.3) ~ 1, TRUE ~ 0),
  
  moderate.unilat = case_when((raab$right_distance_acuity_presenting==1.0 & raab$left_distance_acuity_presenting==0.3)  |
                                (raab$left_distance_acuity_presenting==1.0 & raab$right_distance_acuity_presenting==0.3) ~ 1, TRUE ~ 0),
  
  mild.unilat = case_when((raab$right_distance_acuity_presenting==0.47 & raab$left_distance_acuity_presenting==0.3)  |
                            (raab$left_distance_acuity_presenting==0.47 & raab$right_distance_acuity_presenting==0.3) ~ 1, TRUE ~ 0),
  
)


raab <- raab %>% mutate(
  
  unilat.vi = case_when(mild.unilat==1 ~ "mild.unilat", moderate.unilat==1 ~ "moderate.unilat", severe.unilat==1 ~ "severe.unilat", blind.unilat==1 ~ "blind.unilat")
  
)

unilat.vi<-c("mild.unilat","moderate.unilat","severe.unilat","blind.unilat")

#Cataract surgical outcome variables

raab <- raab %>% mutate(
  
  right_operable_612 = case_when((raab$lens_status_right=="lens_status_opacity" & raab$poor_vision_cause_right=="poor_vision_cause_cataract_untreated" & (raab$right_distance_acuity_pinhole>=0.47)) ~ 1, TRUE ~ 0),
  left_operable_612 = case_when((raab$lens_status_left=="lens_status_opacity" & raab$poor_vision_cause_left=="poor_vision_cause_cataract_untreated" & (raab$left_distance_acuity_pinhole>=0.47)) ~ 1, TRUE ~ 0),
  right_operable_618 = case_when((raab$lens_status_right=="lens_status_opacity" & raab$poor_vision_cause_right=="poor_vision_cause_cataract_untreated" & (raab$right_distance_acuity_pinhole>=1.0)) ~ 1, TRUE ~ 0),
  left_operable_618 = case_when((raab$lens_status_left=="lens_status_opacity" & raab$poor_vision_cause_left=="poor_vision_cause_cataract_untreated" & (raab$left_distance_acuity_pinhole>=1.0)) ~ 1, TRUE ~ 0),
  right_operable_660 = case_when((raab$lens_status_right=="lens_status_opacity" & raab$poor_vision_cause_right=="poor_vision_cause_cataract_untreated" & (raab$right_distance_acuity_pinhole>=1.3)) ~ 1, TRUE ~ 0),
  left_operable_660 = case_when((raab$lens_status_left=="lens_status_opacity" & raab$poor_vision_cause_left=="poor_vision_cause_cataract_untreated" & (raab$left_distance_acuity_pinhole>=1.3)) ~ 1, TRUE ~ 0),
  right_operable_360 = case_when((raab$lens_status_right=="lens_status_opacity" & raab$poor_vision_cause_right=="poor_vision_cause_cataract_untreated" & (raab$right_distance_acuity_pinhole>=1.8)) ~ 1, TRUE ~ 0),
  left_operable_360 = case_when((raab$lens_status_left=="lens_status_opacity" & raab$poor_vision_cause_left=="poor_vision_cause_cataract_untreated" & (raab$left_distance_acuity_pinhole>=1.8)) ~ 1, TRUE ~ 0),
 
#Operated definition: Excludes couched eyes, and non-surgical aphakia and includes "no view of lens" if cataract surgical complications is recorded as cause of poor vision
  
  right_operated = case_when(((raab$lens_status_right=="lens_status_absent" & raab$surgery_type_right!="surgery_type_couching") | (raab$lens_status_right=="lens_status_absent_with_surgery" & raab$surgery_type_right!="surgery_type_couching") | raab$lens_status_right=="lens_status_pseudophakia_no_pco" | raab$lens_status_right=="lens_status_pseudophakia_with_pco" | (raab$lens_status_right=="lens_status_no_view" & raab$poor_vision_cause_right=="poor_vision_cause_cataract_surgical_complications")) ~ 1, TRUE ~ 0),
  left_operated = case_when(((raab$lens_status_left=="lens_status_absent" & raab$surgery_type_left!="surgery_type_couching") | (raab$lens_status_left=="lens_status_absent_with_surgery" & raab$surgery_type_left!="surgery_type_couching") | raab$lens_status_left=="lens_status_pseudophakia_no_pco" | raab$lens_status_left=="lens_status_pseudophakia_with_pco" | (raab$lens_status_left=="lens_status_no_view" & raab$poor_vision_cause_left=="poor_vision_cause_cataract_surgical_complications")) ~ 1, TRUE ~ 0)
)

# CSC and eCSC variables - new eCSC definition entered 16.06.22

raab <- raab %>% 
  
  mutate( 
    
#eCSC and CSC denominator definition: Any cause of VI in non-operated eyes (with addition of !=operated to define non-operated eye)

    x_case_612 = case_when(((raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=0.47 & raab$left_operated==1) | (raab$right_operated==1 & raab$left_operated!=1 &raab$left_distance_acuity_pinhole>=0.47)) ~ 1, TRUE ~ 0),
    x_case_618 = case_when(((raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.0 & raab$left_operated==1)  | (raab$right_operated==1 & raab$left_operated!=1 &raab$left_distance_acuity_pinhole>=1.0)) ~ 1, TRUE ~ 0),
    x_case_660 = case_when(((raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.3 & raab$left_operated==1)  | (raab$right_operated==1 & raab$left_operated!=1 &raab$left_distance_acuity_pinhole>=1.3)) ~ 1, TRUE ~ 0),
    x_case_360 = case_when(((raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.8 & raab$left_operated==1)  | (raab$right_operated==1 & raab$left_operated!=1 &raab$left_distance_acuity_pinhole>=1.8)) ~ 1, TRUE ~ 0),

    y_case_612 = case_when((raab$right_operated==1 & raab$left_operated==1) ~ 1, TRUE ~ 0),
    z_case_612 = case_when((raab$right_distance_acuity_pinhole>0.3 & raab$left_distance_acuity_pinhole>0.3) & (raab$right_operated!=1 & raab$left_operated!=1) & (raab$right_operable_612==1 | raab$left_operable_612==1) ~ 1, TRUE ~ 0),
    old_z_case_612 = case_when((raab$right_operable_612==1 & raab$left_operable_612==1) ~ 1, TRUE ~ 0),

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
    a_case_612_612 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=0.3) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=0.47) | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=0.3) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=0.47)  ~ 1, TRUE ~ 0),
    a_case_612_618 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=0.3) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.0)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=0.3) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.0)  ~ 1, TRUE ~ 0),
    a_case_612_660 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=0.3) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.3)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=0.3) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.3)  ~ 1, TRUE ~ 0),
    a_case_612_360 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=0.3) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.8)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=0.3) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.8)  ~ 1, TRUE ~ 0),

    a_case_618_612 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=0.47) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=0.47) | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=0.47) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=0.47)  ~ 1, TRUE ~ 0),
    a_case_618_618 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=0.47) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.0)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=0.47) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.0)  ~ 1, TRUE ~ 0),
    a_case_618_660 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=0.47) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.3)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=0.47) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.3)  ~ 1, TRUE ~ 0),
    a_case_618_360 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=0.47) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.8)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=0.47) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.8)  ~ 1, TRUE ~ 0),

#    a_case_660_612 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=1.0) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=0.47) | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=1.0) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=0.47)  ~ 1, TRUE ~ 0),
#    a_case_660_618 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=1.0) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.0)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=1.0) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.0)  ~ 1, TRUE ~ 0),
#    a_case_660_660 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=1.0) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.3)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=1.0) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.3)  ~ 1, TRUE ~ 0),
#    a_case_660_360 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=1.0) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.8)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=1.0) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.8)  ~ 1, TRUE ~ 0),

#    a_case_360_612 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=1.3) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=0.47) | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=1.3) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=0.47)  ~ 1, TRUE ~ 0),
#    a_case_360_618 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=1.3) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.0)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=1.3) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.0)  ~ 1, TRUE ~ 0),
#    a_case_360_660 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=1.3) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.3)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=1.3) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.3)  ~ 1, TRUE ~ 0),
#    a_case_360_360 = case_when(((raab$right_operated==1 & raab$right_distance_acuity_presenting<=1.3) & raab$left_operated!=1 & raab$left_distance_acuity_pinhole>=1.8)  | ((raab$left_operated==1 & raab$left_distance_acuity_presenting<=1.3) & raab$right_operated!=1 & raab$right_distance_acuity_pinhole>=1.8)  ~ 1, TRUE ~ 0),

    b_case_612 = case_when((raab$right_operated==1 & raab$left_operated==1 & pmin(raab$right_distance_acuity_presenting,raab$left_distance_acuity_presenting)<0.47) ~ 1, TRUE ~ 0),
    b_case_618 = case_when((raab$right_operated==1 & raab$left_operated==1 & ifelse(as.numeric(raab$right_distance_acuity_presenting) < as.numeric(raab$left_distance_acuity_presenting),(raab$right_distance_acuity_presenting<1.0),(raab$left_distance_acuity_presenting<1.0))) ~ 1, TRUE ~ 0),
#    b_case_660 = case_when((raab$right_operated==1 & raab$left_operated==1 & pmin(raab$right_distance_acuity_presenting,raab$left_distance_acuity_presenting)<1.3) ~ 1, TRUE ~ 0),
#    b_case_360 = case_when((raab$right_operated==1 & raab$left_operated==1 & ifelse(as.numeric(raab$right_distance_acuity_presenting) < as.numeric(raab$left_distance_acuity_presenting),(raab$right_distance_acuity_presenting<1.8),(raab$left_distance_acuity_presenting<1.8))) ~ 1, TRUE ~ 0)
    
  )

# New vars for cataract VI at the person level (aligned with new eCSC definition denominator = unmet need for cataract surgery)
raab$cataract.blind <- (raab$z_case_360)+0
raab$cataract.severe.vi <- (raab$z_case_660)+0
raab$cataract.moderate.vi <- (raab$z_case_618)+0
raab$cataract.mild.vi <- (raab$z_case_612)+0

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
  
  right.borderline.oc = case_when(raab$right_distance_acuity_presenting==0.47 | raab$right_distance_acuity_presenting==1.0 ~ 1, TRUE ~ 0),
  
  right.good.oc = case_when(raab$right_distance_acuity_presenting==0.3 ~ 1, TRUE ~ 0),
  
  left.poor.oc = case_when(raab$left_distance_acuity_presenting>1.0 ~ 1, TRUE ~ 0), 
  
  left.borderline.oc = case_when(raab$left_distance_acuity_presenting==0.47 | raab$left_distance_acuity_presenting==1.0 ~ 1, TRUE ~ 0),
  
  left.good.oc = case_when(raab$left_distance_acuity_presenting==0.3 ~ 1, TRUE ~ 0),
  
)

raab <- raab %>% mutate(
  
  right.oc.levels = case_when(right.good.oc==1 ~ "right.good.oc", right.borderline.oc==1 ~ "right.borderline.oc", right.poor.oc==1 ~ "right.poor.oc"),
  left.oc.levels = case_when(left.good.oc==1 ~ "left.good.oc", left.borderline.oc==1 ~ "left.borderline.oc", left.poor.oc==1 ~ "left.poor.oc")
  
)

raab <- raab %>% mutate(
  
  right.poor.oc.pinva = case_when(raab$right_distance_acuity_pinhole>1.0 ~ 1, TRUE ~ 0), 
  
  right.borderline.oc.pinva = case_when(raab$right_distance_acuity_pinhole==0.47 | raab$right_distance_acuity_pinhole==1.0 ~ 1, TRUE ~ 0),
  
  right.good.oc.pinva = case_when(raab$right_distance_acuity_pinhole==0.3 ~ 1, TRUE ~ 0),
  
  left.poor.oc.pinva = case_when(raab$left_distance_acuity_pinhole>1.0 ~ 1, TRUE ~ 0), 
  
  left.borderline.oc.pinva = case_when(raab$left_distance_acuity_pinhole==0.47 | raab$left_distance_acuity_pinhole==1.0 ~ 1, TRUE ~ 0),
  
  left.good.oc.pinva = case_when(raab$left_distance_acuity_pinhole==0.3 ~ 1, TRUE ~ 0),
  
)

raab <- raab %>% mutate(
  
  right.oc.pinva.levels = case_when(right.good.oc.pinva==1 ~ "right.good.oc.pinva", right.borderline.oc.pinva==1 ~ "right.borderline.oc.pinva", right.poor.oc.pinva==1 ~ "right.poor.oc.pinva"),
  left.oc.pinva.levels = case_when(left.good.oc.pinva==1 ~ "left.good.oc.pinva", left.borderline.oc.pinva==1 ~ "left.borderline.oc.pinva", left.poor.oc.pinva==1 ~ "left.poor.oc.pinva")
  
)

#Surgery places

surgery_places <- c("surgery_place_camp_improvised","surgery_place_gov_hospital","surgery_place_private_hospital","surgery_place_traditional","surgery_place_voluntary_hospital")

#cataract surgery types and outcomes

raab <- raab %>% mutate(
  
  right.operated.eyes.denom = case_when((lens_status_right=="lens_status_absent" | lens_status_right=="lens_status_absent_with_surgery" | lens_status_right=="lens_status_pseudophakia_no_pco" | lens_status_right=="lens_status_pseudophakia_with_pco" | (raab$lens_status_right=="lens_status_no_view" & raab$poor_vision_cause_right=="poor_vision_cause_cataract_surgical_complications")) ~ 1, TRUE ~0),
  left.operated.eyes.denom = case_when((lens_status_left=="lens_status_absent" | lens_status_left=="lens_status_absent_with_surgery" | lens_status_left=="lens_status_pseudophakia_no_pco" | lens_status_left=="lens_status_pseudophakia_with_pco" | (raab$lens_status_left=="lens_status_no_view" & raab$poor_vision_cause_left=="poor_vision_cause_cataract_surgical_complications")) ~ 1, TRUE ~0)
  
)

#Refractive error 

raab <- raab %>% mutate(
  
  better.eye.ucva = pmin(raab$right_distance_acuity_uncorrected, raab$left_distance_acuity_uncorrected),
  
  better.eye.cva = pmin(raab$right_distance_acuity_corrected, raab$left_distance_acuity_corrected),
  
  better.eye.pva = pmin(raab$right_distance_acuity_presenting, raab$left_distance_acuity_presenting),
  
  better.eye.pinva = pmin(raab$right_distance_acuity_pinhole, raab$left_distance_acuity_pinhole)
  
)

raab <- raab %>% mutate(
  
  right.re.blind = case_when(raab$poor_vision_cause_right=="poor_vision_cause_uncorrected_refractive_error" & raab$right_distance_acuity_presenting>=1.8 ~ 1, TRUE ~ 0), 
  
  right.re.severe.vi = case_when(raab$poor_vision_cause_right=="poor_vision_cause_uncorrected_refractive_error" & raab$right_distance_acuity_presenting==1.3 ~ 1, TRUE ~ 0),
  
  right.re.moderate.vi = case_when(raab$poor_vision_cause_right=="poor_vision_cause_uncorrected_refractive_error" & raab$right_distance_acuity_presenting==1.0 ~ 1, TRUE ~ 0),
  
  right.re.mild.vi = case_when(raab$poor_vision_cause_right=="poor_vision_cause_uncorrected_refractive_error" & raab$right_distance_acuity_presenting==0.47 ~ 1, TRUE ~ 0),

  left.re.blind = case_when(raab$poor_vision_cause_left=="poor_vision_cause_uncorrected_refractive_error" & raab$left_distance_acuity_presenting>=1.8 ~ 1, TRUE ~ 0), 
  
  left.re.severe.vi = case_when(raab$poor_vision_cause_left=="poor_vision_cause_uncorrected_refractive_error" & raab$left_distance_acuity_presenting==1.3 ~ 1, TRUE ~ 0),
  
  left.re.moderate.vi = case_when(raab$poor_vision_cause_left=="poor_vision_cause_uncorrected_refractive_error" & raab$left_distance_acuity_presenting==1.0 ~ 1, TRUE ~ 0),
  
  left.re.mild.vi = case_when(raab$poor_vision_cause_left=="poor_vision_cause_uncorrected_refractive_error" & raab$left_distance_acuity_presenting==0.47 ~ 1, TRUE ~ 0)
)

raab <- raab %>% mutate(
  
  re.blind = case_when(raab$better.eye.pva>1.3 & raab$better.eye.pinva==0.3 & poor_vision_cause_principle=="poor_vision_cause_uncorrected_refractive_error" ~ 1, TRUE ~ 0),
  
  re.severe.vi = case_when(raab$better.eye.pva==1.3 & raab$better.eye.pinva==0.3 & poor_vision_cause_principle=="poor_vision_cause_uncorrected_refractive_error" ~ 1, TRUE ~ 0),
  
  re.moderate.vi = case_when(raab$better.eye.pva==1.0 & raab$better.eye.pinva==0.3 & poor_vision_cause_principle=="poor_vision_cause_uncorrected_refractive_error" ~ 1, TRUE ~ 0),
  
  re.mild.vi = case_when(raab$better.eye.pva==0.47 & raab$better.eye.pinva==0.3 & poor_vision_cause_principle=="poor_vision_cause_uncorrected_refractive_error" ~ 1, TRUE ~ 0)

)

raab <- raab %>% mutate(
  
  re.vi.levels = case_when(re.mild.vi==1 ~ "re.mild.vi", re.moderate.vi==1 ~ "re.moderate.vi", re.severe.vi==1 ~ "re.severe.vi", re.blind==1 ~ "re.blind")

)

# Variable for prevalence of refractive error, regardless of whether corrected or uncorrected
raab <- raab %>% mutate(
  ref.error = case_when(
    better.eye.ucva>0.3 & (better.eye.cva==0.3 | better.eye.pinva==0.3) ~1, TRUE ~ 0)
  )


#eREC/REC variables

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
  
  aa_case = case_when(raab$spectacles_used_distance==TRUE & raab$better.eye.ucva>0.3 & raab$better.eye.cva==0.3 ~ 1, TRUE ~ 0)

  )

raab <- raab %>% mutate(
  
  bb_case = case_when(raab$aa_case==0 & raab$spectacles_used_distance==TRUE & raab$better.eye.ucva>0.3 & raab$better.eye.cva>0.3 & raab$better.eye.pinva==0.3 ~ 1, TRUE ~ 0),
  cc_case = case_when(raab$spectacles_used_distance==FALSE & raab$better.eye.ucva>0.3 & raab$better.eye.pinva==0.3 ~ 1, TRUE ~ 0)

  )

raab <- raab %>% mutate(
  
  dd_case = case_when(raab$exam_status=="exam_status_examined" & (raab$aa_case!=1 & raab$bb_case!=1 & raab$cc_case!=1) ~ 1, TRUE ~ 0)

  )

# Near vision screening analysis for RAAB7
# 27 Oct 2023 - v1
# 14 Nov 2023 - v2: new Peek var names and output values

if(exists('binocular_near_corrected_result',where=raab)){

# Generate a "presenting" test outcome (assuming Peek aren't making one, delete if they are)
raab <- raab %>% mutate(
    
  binocular_near_presenting_result = case_when(
    spectacles_used_near==TRUE ~ binocular_near_corrected_result,
    spectacles_used_near==FALSE ~ binocular_near_uncorrected_result)
    
)

# Define near VI based on presenting near VA (only one level, binary screening test at N6 threshold)
  
raab <- raab %>% mutate(
  
  near.vi = case_when(raab$binocular_near_presenting_result=="acuity_evaluation_result_fail" ~ 1, TRUE ~ 0)
  
)

# Define near eREC terms (for "spectacle coverage for near vision impairment due to presbyopia")

# a=individuals with UCVA <N6 at 40 cm in the better eye who present with spectacles for near vision and whose PVA
# is ≥N6 in the better eye (met need) [NB the WHO paper box does not include "distance BCVA ≥6/12* in at least one eye" but the supp material flow chart does]
# 
# b=individuals with distance BCVA ≥6/12* in at least one eye who present with spectacles for near vision and whose
# PVA is <N6 in the better eye (undermet need)
# 
# c=individuals with distance BCVA ≥6/12 in at least one eye who do not have correction for near vision and whose
# UCVA is <N6 in the better eye (unmet need)

# Only individuals with distance BCVA ≥6/12 will be considered in order to exclude those with reduced near vision not due to other causes.

# ee_case = met need
# ff_case = undermet need
# gg_case = unmet need
# hh_case = no need

raab <- raab %>% mutate(
  
  ee_case = case_when(raab$better.eye.pinva==0.3 & raab$spectacles_used_near==TRUE & raab$binocular_near_uncorrected_result=="acuity_evaluation_result_fail" & raab$binocular_near_corrected_result=="acuity_evaluation_result_pass" ~ 1, TRUE ~ 0)
  
)

raab <- raab %>% mutate(
  
  ff_case = case_when(raab$ee_case==0 & raab$better.eye.pinva==0.3 & raab$spectacles_used_near==TRUE & raab$binocular_near_uncorrected_result=="acuity_evaluation_result_fail" & raab$binocular_near_corrected_result=="acuity_evaluation_result_fail" ~ 1, TRUE ~ 0),
  gg_case = case_when(raab$better.eye.pinva==0.3 & raab$spectacles_used_near==FALSE & raab$binocular_near_uncorrected_result=="acuity_evaluation_result_fail" ~ 1, TRUE ~ 0),
  
  hh_case = case_when(raab$binocular_near_uncorrected_result=="acuity_evaluation_result_pass" ~ 1, TRUE ~ 0)
)
}else{raab$near.vi<-NA}

#Washington Group Questions (Disability module) variables
#Modified to include the short set enhanced additional questions on upper body and mental health

#Domain-specific disability

raab <- raab %>% mutate(
 
   wgq.dis.see = case_when(raab$wg_difficulty_seeing=="wg_answer_alot" | raab$wg_difficulty_seeing=="wg_answer_cannot" ~ 1, TRUE ~ 0),
   wgq.dis.hear = case_when(raab$wg_difficulty_hearing=="wg_answer_alot" | raab$wg_difficulty_hearing=="wg_answer_cannot" ~ 1, TRUE ~ 0),
   wgq.dis.mob = case_when(raab$wg_difficulty_mobility=="wg_answer_alot" | raab$wg_difficulty_mobility=="wg_answer_cannot" ~ 1, TRUE ~ 0),
   wgq.dis.mem = case_when(raab$wg_difficulty_memory=="wg_answer_alot" | raab$wg_difficulty_memory=="wg_answer_cannot" ~ 1, TRUE ~ 0),
   wgq.dis.comm = case_when(raab$wg_difficulty_communication=="wg_answer_alot" | raab$wg_difficulty_communication=="wg_answer_cannot" ~ 1, TRUE ~ 0),
   wgq.dis.self = case_when(raab$wg_difficulty_selfcare=="wg_answer_alot" | raab$wg_difficulty_selfcare=="wg_answer_cannot" ~ 1, TRUE ~ 0),
   wgq.dis.upbod.str = case_when(raab$wg_difficulty_upperbody_strength=="wg_answer_alot" | raab$wg_difficulty_upperbody_strength=="wg_answer_cannot" ~ 1, TRUE ~ 0),
   wgq.dis.upbod.dex = case_when(raab$wg_difficulty_upperbody_dexterity=="wg_answer_alot" | raab$wg_difficulty_upperbody_dexterity=="wg_answer_cannot" ~ 1, TRUE ~ 0),
   wgq.dis.anx = case_when((raab$wg_difficulty_anxiety_frequency=="wg_answer_frequency_daily" & (wg_difficulty_anxiety_intensity=="wg_answer_intensity_medium" | wg_difficulty_anxiety_intensity=="wg_answer_intensity_lot"))  | (raab$wg_difficulty_anxiety_frequency=="wg_answer_frequency_weekly" & raab$wg_difficulty_anxiety_intensity=="wg_answer_intensity_lot") ~ 1, TRUE ~ 0),
   wgq.dis.dep = case_when((raab$wg_difficulty_depression_frequency=="wg_answer_frequency_daily" & (wg_difficulty_depression_intensity=="wg_answer_intensity_medium" | wg_difficulty_depression_intensity=="wg_answer_intensity_lot"))  | (raab$wg_difficulty_depression_frequency=="wg_answer_frequency_weekly" & raab$wg_difficulty_depression_intensity=="wg_answer_intensity_lot") ~ 1, TRUE ~ 0)
 )
 
#Disability in any domain and any domain excluding seeing
 raab <- raab %>% mutate(
 
   wgq.dis.any = case_when(wgq.dis.see==1 | wgq.dis.hear==1 | wgq.dis.mob==1 | wgq.dis.mem==1 | wgq.dis.comm==1 | wgq.dis.self==1 | wgq.dis.upbod.str==1 | wgq.dis.upbod.dex==1 | wgq.dis.anx==1 | wgq.dis.dep==1 ~ 1, TRUE ~ 0),
   wgq.dis.nonvi = case_when(wgq.dis.hear==1 | wgq.dis.mob==1 | wgq.dis.mem==1 | wgq.dis.comm==1 | wgq.dis.self==1 | wgq.dis.upbod.str==1 | wgq.dis.upbod.dex==1 | wgq.dis.anx==1 | wgq.dis.dep==1 ~ 1, TRUE ~ 0)
 )
 
 dis.domains<- c("wgq.dis.see", "wgq.dis.hear", "wgq.dis.mob", "wgq.dis.mem", "wgq.dis.comm", "wgq.dis.self", "wgq.dis.upbod.str", "wgq.dis.upbod.dex", "wgq.dis.anx", "wgq.dis.dep", "wgq.dis.any", "wgq.dis.nonvi")

# DR Module variables

# Notes
# diabetes.denom = denominator for reporting DM status among DR module participants via self-reported or RBG consent, excludes anyone not previously diagnosed and not consenting to RBG
# diabetes.new = cases of suspected DM based on RBG result among those not self-reporting history of DM
# diabetes.known.susp = cases of self-reported DM & suspected DM based on RBG result combined
# diabetes.no = cases where no history of DM self-reported and normal RBG result among diabetes.denom
# dr.exam.denom = denominator for reporting fundus examination results

# dr.response.cascade <-c("Enrolled","Examined","Diabetes status assessed", "Known or suspected diabetes", "Consented dilated examination")
dr.response.cascade <-c("Enrolled","Examined","Diabetes status assessed")
dr.response.cascade.b <- (c("Known or suspected diabetes", "Known", "Suspected", "Consented dilated examination"))

if(is.logical(raab$dr_diabetes_known)){
    raab <- raab %>% mutate(
    
      diabetes.denom = case_when(dr_diabetes_known==TRUE | dr_diabetes_blood_consent==TRUE ~1, TRUE~0),
      diabetes.new = case_when((dr_diabetes_known==FALSE & dr_diabetes_blood_consent==TRUE & dr_diabetes_blood_sugar>=200) ~1, TRUE~0),
      diabetes.known = case_when(dr_diabetes_known==TRUE ~1, TRUE~0),
      diabetes.known.susp = case_when((dr_diabetes_known==TRUE | diabetes.new==1) ~1, TRUE~0),
      dr.exam.denom = case_when(diabetes.known.susp==1 & (dr_retinopathy_method_right=="dr_retinopathy_method_dilatation_fundoscopy" | dr_retinopathy_method_right=="dr_retinopathy_method_fundus_camera")  ~1, TRUE~0)
    )}else{
  
    raab <- raab %>% mutate(
    
      diabetes.denom = case_when(dr_diabetes_known=="true" | dr_diabetes_blood_consent=="true" ~1, TRUE~0),
      diabetes.new = case_when((dr_diabetes_known=="false" & dr_diabetes_blood_consent=="true" & dr_diabetes_blood_sugar>=200) ~1, TRUE~0),
      diabetes.known = case_when(dr_diabetes_known=="true" ~1, TRUE~0),
      diabetes.known.susp = case_when((dr_diabetes_known=="true" | diabetes.new==1) ~1, TRUE~0),
      dr.exam.denom = case_when(diabetes.known.susp==1 & (dr_retinopathy_method_right=="dr_retinopathy_method_dilatation_fundoscopy" | dr_retinopathy_method_right=="dr_retinopathy_method_fundus_camera")  ~1, TRUE~0)
    )}

raab <- raab %>% mutate(
diabetes.no = case_when((diabetes.denom==1 & diabetes.known.susp==0) ~1, TRUE~0))

# Made grades numeric so more easily treated as ordinal values
retinopathy.grade <- c("dr_retinopathy_grade_none", "dr_retinopathy_grade_mild", "dr_retinopathy_grade_observable", "dr_retinopathy_grade_referable", "dr_retinopathy_grade_proliferative", "dr_retinopathy_grade_not_visualised")
dr.ret.grade.person <- c(1,2,3,4,5,0)

raab <- raab %>% mutate(  
  dr.ret.grade.right = case_when(
    dr_retinopathy_grade_right=="dr_retinopathy_grade_none" ~1,
    dr_retinopathy_grade_right=="dr_retinopathy_grade_mild" ~2,
    dr_retinopathy_grade_right=="dr_retinopathy_grade_observable" ~3,
    dr_retinopathy_grade_right=="dr_retinopathy_grade_referable" ~4,
    dr_retinopathy_grade_right=="dr_retinopathy_grade_proliferative" ~5,
    dr_retinopathy_grade_right=="dr_retinopathy_grade_not_visualised" ~0),
  dr.ret.grade.left = case_when(
    dr_retinopathy_grade_left=="dr_retinopathy_grade_none" ~1,
    dr_retinopathy_grade_left=="dr_retinopathy_grade_mild" ~2,
    dr_retinopathy_grade_left=="dr_retinopathy_grade_observable" ~3,
    dr_retinopathy_grade_left=="dr_retinopathy_grade_referable" ~4,
    dr_retinopathy_grade_left=="dr_retinopathy_grade_proliferative" ~5,
    dr_retinopathy_grade_left=="dr_retinopathy_grade_not_visualised" ~0),
  dr.mac.grade.right = case_when(
    dr_maculopathy_grade_right=="dr_maculopathy_grade_none" ~1,
    dr_maculopathy_grade_right=="dr_maculopathy_grade_observable" ~2,
    dr_maculopathy_grade_right=="dr_maculopathy_grade_referable" ~3,
    dr_maculopathy_grade_right=="dr_maculopathy_grade_not_visualised" ~0),
  dr.mac.grade.left = case_when(
    dr_maculopathy_grade_left=="dr_maculopathy_grade_none" ~1,
    dr_maculopathy_grade_left=="dr_maculopathy_grade_observable" ~2,
    dr_maculopathy_grade_left=="dr_maculopathy_grade_referable" ~3,
    dr_maculopathy_grade_left=="dr_maculopathy_grade_not_visualised" ~0),
  
  dr.ret.grade.person = pmax(dr.ret.grade.right, dr.ret.grade.left),
  dr.mac.grade.person = pmax(dr.mac.grade.right, dr.mac.grade.left),
  
  dr.ret.any.person = case_when((dr.ret.grade.right>1 | dr.ret.grade.left>1) ~1, TRUE~0),
  dr.mac.any.person = case_when((dr.mac.grade.right>1 | dr.mac.grade.left>1) ~1, TRUE~0),
  dr.ret.mac.any.person = case_when(dr.ret.any.person==1 | dr.mac.any.person==1 ~1, TRUE~0),
  dr.stdr.any.person = case_when((dr.ret.grade.right==5 | dr.ret.grade.left==5 | dr.mac.grade.right==3 | dr.mac.grade.left==3) ~1, TRUE~0),
  dr.laser.person = case_when(dr_laser_photocoagulation_scars_right=="dr_laser_photocoagulation_scars_macular" | dr_laser_photocoagulation_scars_right=="dr_laser_photocoagulation_scars_pan_retinal" | dr_laser_photocoagulation_scars_right=="dr_laser_photocoagulation_scars_pan_retinal_and_macular" |
                                dr_laser_photocoagulation_scars_left=="dr_laser_photocoagulation_scars_macular" | dr_laser_photocoagulation_scars_left=="dr_laser_photocoagulation_scars_pan_retinal" | dr_laser_photocoagulation_scars_left=="dr_laser_photocoagulation_scars_pan_retinal_and_macular" ~1, TRUE~0)
)

dr.last.exam <- c("dr_diabetic_last_exam_none", "dr_diabetic_last_exam_0_12_months", "dr_diabetic_last_exam_13_24_months", "dr_diabetic_last_exam_over_24_months")

# Subjective SEP variables
# Create three levels of income from five response options
# raab <- raab %>% mutate(
#   income = case_when(
#     (sep_income_sufficiency=="sep_income_sufficiency_borrow" | sep_income_sufficiency=="sep_income_sufficiency_savings") ~1,
#     sep_income_sufficiency=="sep_income_sufficiency_enough_just" ~2,
#     (sep_income_sufficiency=="sep_income_sufficiency_enough_save" | sep_income_sufficiency=="sep_income_sufficiency_enough_building_savings") ~3, TRUE~0)
# )
# 
# Food.Status <- c("sep_food_adequacy_less","sep_food_adequacy_adequate","sep_food_adequacy_more") 
# Income.Status <- c(1,2,3) 