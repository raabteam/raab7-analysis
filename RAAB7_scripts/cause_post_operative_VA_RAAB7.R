#Causes of post-operative presenting VA <6/12 in cataract operated eyes with borderline and poor outcomes 
#AH 15.10.24
#IM 07.11.24 modified to give row pct instead of col pct 

causes.po.va <- data.frame(oc.tab$oc.levels[c(1,2)])
causes.po.va[,2:13] <- NA

names(causes.po.va) <- c("oc.levels",
                         
                         "case.selection.n",
                         "case.selection.pct",
                         
                         "intraoperative.complications.n",
                         "intraoperative.complications.pct",
                         
                         "pco.n",
                         "pco.pct",
                         
                         "other.sequelae.n",
                         "other.sequelae.pct",
                         
                         "refractive.error.n",
                         "refractive.error.pct",
                         
                         "total.n",
                         "total.pct")


for (i in 1:length(causes.po.va$oc.levels)) {
  
  causes.po.va$case.selection.n[i] <- sum(raab$postop.eyes.right.denom[raab$right.oc.levels == oc.tab$right.oc.levels[i] & raab$surgery_ocular_comorbidity.re == 1], na.rm = TRUE) +
    sum(raab$postop.eyes.left.denom[raab$left.oc.levels == oc.tab$left.oc.levels[i] & raab$surgery_ocular_comorbidity.le == 1], na.rm = TRUE)
  
  causes.po.va$intraoperative.complications.n[i] <- sum(raab$postop.eyes.right.denom[raab$right.oc.levels == oc.tab$right.oc.levels[i] & raab$surgery_op_comp.re == 1], na.rm = TRUE) +
    sum(raab$postop.eyes.left.denom[raab$left.oc.levels == oc.tab$left.oc.levels[i] & raab$surgery_op_comp.le == 1], na.rm = TRUE)
  
  causes.po.va$pco.n[i] <- sum(raab$postop.eyes.right.denom[raab$right.oc.levels == oc.tab$right.oc.levels[i] & raab$surgery_pco.re == 1], na.rm = TRUE) +
    sum(raab$postop.eyes.left.denom[raab$left.oc.levels == oc.tab$left.oc.levels[i] & raab$surgery_pco.le == 1], na.rm = TRUE)
  
  causes.po.va$other.sequelae.n[i] <- sum(raab$postop.eyes.right.denom[raab$right.oc.levels == oc.tab$right.oc.levels[i] & raab$surgery_other_seq.re == 1], na.rm = TRUE) +
    sum(raab$postop.eyes.left.denom[raab$left.oc.levels == oc.tab$left.oc.levels[i] & raab$surgery_other_seq.le == 1], na.rm = TRUE)
  
  causes.po.va$refractive.error.n[i] <- sum(raab$postop.eyes.right.denom[raab$right.oc.levels == oc.tab$right.oc.levels[i] & raab$surgery_ref_err.re == 1], na.rm = TRUE) +
    sum(raab$postop.eyes.left.denom[raab$left.oc.levels == oc.tab$left.oc.levels[i] & raab$surgery_ref_err.le == 1], na.rm = TRUE)
  

  row_total <- sum(causes.po.va[i, c("case.selection.n", "intraoperative.complications.n", "pco.n", "other.sequelae.n", "refractive.error.n")], na.rm = TRUE)
  causes.po.va$total.n[i] <- row_total 
  
  causes.po.va$case.selection.pct[i] <- (causes.po.va$case.selection.n[i] / row_total) * 100
  causes.po.va$intraoperative.complications.pct[i] <- (causes.po.va$intraoperative.complications.n[i] / row_total) * 100
  causes.po.va$pco.pct[i] <- (causes.po.va$pco.n[i] / row_total) * 100
  causes.po.va$other.sequelae.pct[i] <- (causes.po.va$other.sequelae.n[i] / row_total) * 100
  causes.po.va$refractive.error.pct[i] <- (causes.po.va$refractive.error.n[i] / row_total) * 100
  causes.po.va$total.pct[i] <- 100
}

causes.po.va.pcts <- grep("pct", names(causes.po.va))
causes.po.va[, causes.po.va.pcts] <- round(causes.po.va[, causes.po.va.pcts], 1)
causes.po.va[, causes.po.va.pcts] <- format(causes.po.va[, causes.po.va.pcts], nsmall = 1)


