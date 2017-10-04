 newdata <- read.table("new.rdata",header=TRUE,colClasses="character")
 ## Missing value code is NA
 ## Change file name if needed
 ## Predicting class
 predicted <- function(){
 catvalues <- c("10", "2", "23", "33")
 if(STATE %in% catvalues){
 catvalues <- c("1", "10", "12", "2", "4", "7")
 if(OCCUCOD2 %in% catvalues){
 nodeid <-  4
 predict <- "4400"
 } else {
 catvalues <- c("13", "14", "15")
 if(EDUCA2 %in% catvalues){
 nodeid <-  10
 predict <- "860"
 } else {
 nodeid <-  11
 predict <- "900"
 }
 }
 } else {
 catvalues <- c("12", "16", "17", "34", "4", "45", "47", "8")
 if(STATE %in% catvalues){
 catvalues <- c("2", "3", "5")
 if(INCOMEY2 %in% catvalues){
 nodeid <-  12
 predict <- "100"
 } else {
 nodeid <-  13
 predict <- "50"
 }
 } else {
 catvalues <- c("13", "15", "25", "26", "27", "31", "32", "36", "39",
 "42", "49", "53", "55")
 if(STATE %in% catvalues){
 catvalues <- c("13", "53")
 if(STATE %in% catvalues){
 nodeid <-  28
 predict <- "100"
 } else {
 catvalues <- c("4", "5", "7", "8")
 if(EARNCOMP %in% catvalues){
 nodeid <-  58
 predict <- "1"
 } else {
 catvalues <- c("3", "5")
 if(MARITAL1 %in% catvalues){
 catvalues <- c("12", "2", "5", "6", "7")
 if(OCCUCOD1 %in% catvalues){
 catvalues <- c("15", "25", "36", "42", "49", "55")
 if(STATE %in% catvalues){
 nodeid <-  472
 predict <- "3"
 } else {
 nodeid <-  473
 predict <- "1"
 }
 } else {
 nodeid <-  237
 predict <- "10"
 }
 } else {
 nodeid <-  119
 predict <- "20"
 }
 }
 }
 } else {
 catvalues <- c("13", "15", "25", "26", "27", "31", "32", "36", "39",
 catvalues <- c("1313", "1420")
 if(PSU %in% catvalues){
 nodeid <-  30
 predict <- "2000"
 } else {
 catvalues <- c("11", "20", "24")
 if(STATE %in% catvalues){
 nodeid <-  62
 predict <- "30"
 } else {
 nodeid <-  63
 predict <- "200"
 }
 }
 }
 }
 }
 return(c(nodeid,predict))
 }
 ###z <- read.table("fit9.fit",header=TRUE)
 ###noerr <- TRUE
 for(i in 1:dim(newdata)[1]){
 DIRACC <- as.character(newdata$DIRACC[i])
 DIRACC_ <- as.character(newdata$DIRACC_[i])
 AGE_REF_ <- as.character(newdata$AGE_REF_[i])
 AGE2_ <- as.character(newdata$AGE2_[i])
 BATHRMQ_ <- as.character(newdata$BATHRMQ_[i])
 BEDR_OMQ <- as.character(newdata$BEDR_OMQ[i])
 BUILDING <- as.character(newdata$BUILDING[i])
 CUTENURE <- as.character(newdata$CUTENURE[i])
 EARNCOMP <- as.character(newdata$EARNCOMP[i])
 EDUCA2 <- as.character(newdata$EDUCA2[i])
 EDUCA2_ <- as.character(newdata$EDUCA2_[i])
 FAM_TYPE <- as.character(newdata$FAM_TYPE[i])
 FAMT_EDX <- as.character(newdata$FAMT_EDX[i])
 FEDR_NDX <- as.character(newdata$FEDR_NDX[i])
 FEDTAXX_ <- as.character(newdata$FEDTAXX_[i])
 FGOV_ETX <- as.character(newdata$FGOV_ETX[i])
 FINCAT_X <- as.character(newdata$FINCAT_X[i])
 FINCBT_X <- as.character(newdata$FINCBT_X[i])
 FIND_ETX <- as.character(newdata$FIND_ETX[i])
 FJSS_EDX <- as.character(newdata$FJSS_EDX[i])
 FPRI_ENX <- as.character(newdata$FPRI_ENX[i])
 FSAL_RYX <- as.character(newdata$FSAL_RYX[i])
 FSLTAXX_ <- as.character(newdata$FSLTAXX_[i])
 HLFB_THQ <- as.character(newdata$HLFB_THQ[i])
 INC__RS1 <- as.character(newdata$INC__RS1[i])
 INC__RS2 <- as.character(newdata$INC__RS2[i])
 INC__ANK <- as.character(newdata$INC__ANK[i])
 INCNONW1 <- as.character(newdata$INCNONW1[i])
 INCN_NW1 <- as.character(newdata$INCN_NW1[i])
 INCNONW2 <- as.character(newdata$INCNONW2[i])
 INCN_NW2 <- as.character(newdata$INCN_NW2[i])
 INCOMEY1 <- as.character(newdata$INCOMEY1[i])
 INCO_EY1 <- as.character(newdata$INCO_EY1[i])
 INCOMEY2 <- as.character(newdata$INCOMEY2[i])
 INCO_EY2 <- as.character(newdata$INCO_EY2[i])
 INCW_EK2 <- as.character(newdata$INCW_EK2[i])
 MISC_AXX <- as.character(newdata$MISC_AXX[i])
 LUMP_UMX <- as.character(newdata$LUMP_UMX[i])
 MARITAL1 <- as.character(newdata$MARITAL1[i])
 OCCUCOD1 <- as.character(newdata$OCCUCOD1[i])
 OCCU_OD1 <- as.character(newdata$OCCU_OD1[i])
 OCCUCOD2 <- as.character(newdata$OCCUCOD2[i])
 OCCU_OD2 <- as.character(newdata$OCCU_OD2[i])
 OTHR_NCX <- as.character(newdata$OTHR_NCX[i])
 PRINEARN <- as.character(newdata$PRINEARN[i])
 QINTRVMO <- as.character(newdata$QINTRVMO[i])
 QINTRVYR <- as.character(newdata$QINTRVYR[i])
 RACE2 <- as.character(newdata$RACE2[i])
 RACE2_ <- as.character(newdata$RACE2_[i])
 REF_RACE <- as.character(newdata$REF_RACE[i])
 REGION <- as.character(newdata$REGION[i])
 RENT_QVX <- as.character(newdata$RENT_QVX[i])
 RESPSTAT <- as.character(newdata$RESPSTAT[i])
 ROOMSQ_ <- as.character(newdata$ROOMSQ_[i])
 SEX_REF <- as.character(newdata$SEX_REF[i])
 SEX2 <- as.character(newdata$SEX2[i])
 SEX2_ <- as.character(newdata$SEX2_[i])
 SLOC_AXX <- as.character(newdata$SLOC_AXX[i])
 SLRF_NDX <- as.character(newdata$SLRF_NDX[i])
 SMSASTAT <- as.character(newdata$SMSASTAT[i])
 TOTT_PDX <- as.character(newdata$TOTT_PDX[i])
 WELF_REX <- as.character(newdata$WELF_REX[i])
 HHID <- as.character(newdata$HHID[i])
 HHID_ <- as.character(newdata$HHID_[i])
 POV_CY <- as.character(newdata$POV_CY[i])
 POV_CY_ <- as.character(newdata$POV_CY_[i])
 POV_PY <- as.character(newdata$POV_PY[i])
 POV_PY_ <- as.character(newdata$POV_PY_[i])
 SWIM_OOL <- as.character(newdata$SWIM_OOL[i])
 APTMENT_ <- as.character(newdata$APTMENT_[i])
 OFST_ARK <- as.character(newdata$OFST_ARK[i])
 WIND_WAC <- as.character(newdata$WIND_WAC[i])
 CNTR_LAC <- as.character(newdata$CNTR_LAC[i])
 CHILDAGE <- as.character(newdata$CHILDAGE[i])
 STATE <- as.character(newdata$STATE[i])
 ERANKH_ <- as.character(newdata$ERANKH_[i])
 PORCH_ <- as.character(newdata$PORCH_[i])
 WELF_EBX <- as.character(newdata$WELF_EBX[i])
 LUMP_UMB <- as.character(newdata$LUMP_UMB[i])
 LMPS_MBX <- as.character(newdata$LMPS_MBX[i])
 OTHR_NCB <- as.character(newdata$OTHR_NCB[i])
 INTERI <- as.character(newdata$INTERI[i])
 HORREF1 <- as.character(newdata$HORREF1[i])
 HORREF1_ <- as.character(newdata$HORREF1_[i])
 HORREF2 <- as.character(newdata$HORREF2[i])
 HORREF2_ <- as.character(newdata$HORREF2_[i])
 FGOV_ETM <- as.character(newdata$FGOV_ETM[i])
 FPRI_ENM <- as.character(newdata$FPRI_ENM[i])
 PSU <- as.character(newdata$PSU[i])
 HISP_REF <- as.character(newdata$HISP_REF[i])
 HISP2 <- as.character(newdata$HISP2[i])
 BUILT_ <- as.character(newdata$BUILT_[i])
 CRED_INX <- as.character(newdata$CRED_INX[i])
 CREDITB_ <- as.character(newdata$CREDITB_[i])
 CRED_TBX <- as.character(newdata$CRED_TBX[i])
 CREDITX_ <- as.character(newdata$CREDITX_[i])
 CRED_YRX <- as.character(newdata$CRED_YRX[i])
 CREDYRB_ <- as.character(newdata$CREDYRB_[i])
 CRED_RBX <- as.character(newdata$CRED_RBX[i])
 DEFBENRP <- as.character(newdata$DEFBENRP[i])
 DEFB_NRP <- as.character(newdata$DEFB_NRP[i])
 EITC <- as.character(newdata$EITC[i])
 EITC_ <- as.character(newdata$EITC_[i])
 FMLP_YRX <- as.character(newdata$FMLP_YRX[i])
 FS_MTHI_ <- as.character(newdata$FS_MTHI_[i])
 IRAB_ <- as.character(newdata$IRAB_[i])
 IRABX_ <- as.character(newdata$IRABX_[i])
 IRAX_ <- as.character(newdata$IRAX_[i])
 IRAYRB_ <- as.character(newdata$IRAYRB_[i])
 IRAYRBX_ <- as.character(newdata$IRAYRBX_[i])
 IRAYRX_ <- as.character(newdata$IRAYRX_[i])
 LIQD_RBX <- as.character(newdata$LIQD_RBX[i])
 LIQU_DBX <- as.character(newdata$LIQU_DBX[i])
 LIQU_YRB <- as.character(newdata$LIQU_YRB[i])
 LIQU_YRX <- as.character(newdata$LIQU_YRX[i])
 LIQUIDB_ <- as.character(newdata$LIQUIDB_[i])
 LIQUIDX_ <- as.character(newdata$LIQUIDX_[i])
 MEALSPAY <- as.character(newdata$MEALSPAY[i])
 MEAL_PAY <- as.character(newdata$MEAL_PAY[i])
 MLPA_WKX <- as.character(newdata$MLPA_WKX[i])
 MLPY_WKS <- as.character(newdata$MLPY_WKS[i])
 NETRENTB <- as.character(newdata$NETRENTB[i])
 NETR_NTB <- as.character(newdata$NETR_NTB[i])
 NETR_NTX <- as.character(newdata$NETR_NTX[i])
 NETR_TBX <- as.character(newdata$NETR_TBX[i])
 OTHA_TBX <- as.character(newdata$OTHA_TBX[i])
 OTHASTB_ <- as.character(newdata$OTHASTB_[i])
 OTHASTX_ <- as.character(newdata$OTHASTX_[i])
 OTHFINX_ <- as.character(newdata$OTHFINX_[i])
 OTHL_NBX <- as.character(newdata$OTHL_NBX[i])
 OTHL_RBX <- as.character(newdata$OTHL_RBX[i])
 OTHLNYRB <- as.character(newdata$OTHLNYRB[i])
 OTHL_YRB <- as.character(newdata$OTHL_YRB[i])
 OTHL_YRX <- as.character(newdata$OTHL_YRX[i])
 OTHLOAN <- as.character(newdata$OTHLOAN[i])
 OTHLOAN_ <- as.character(newdata$OTHLOAN_[i])
 OTHLONB_ <- as.character(newdata$OTHLONB_[i])
 OTHLONX_ <- as.character(newdata$OTHLONX_[i])
 OTHR_GBX <- as.character(newdata$OTHR_GBX[i])
 OTHREGB <- as.character(newdata$OTHREGB[i])
 OTHREGB_ <- as.character(newdata$OTHREGB_[i])
 OTHREGX_ <- as.character(newdata$OTHREGX_[i])
 OTHS_RBX <- as.character(newdata$OTHS_RBX[i])
 OTHS_YRB <- as.character(newdata$OTHS_YRB[i])
 OTHS_YRX <- as.character(newdata$OTHS_YRX[i])
 RETS_RVB <- as.character(newdata$RETS_RVB[i])
 RETS_RVX <- as.character(newdata$RETS_RVX[i])
 RETS_VBX <- as.character(newdata$RETS_VBX[i])
 RETSURV <- as.character(newdata$RETSURV[i])
 RETSURVB <- as.character(newdata$RETSURVB[i])
 ROYE_TBX <- as.character(newdata$ROYE_TBX[i])
 ROYESTB <- as.character(newdata$ROYESTB[i])
 ROYESTB_ <- as.character(newdata$ROYESTB_[i])
 ROYESTX_ <- as.character(newdata$ROYESTX_[i])
 STCK_RBX <- as.character(newdata$STCK_RBX[i])
 STDN_YRB <- as.character(newdata$STDN_YRB[i])
 STDN_YRX <- as.character(newdata$STDN_YRX[i])
 STDT_RBX <- as.character(newdata$STDT_RBX[i])
 STOC_YRB <- as.character(newdata$STOC_YRB[i])
 STOC_YRX <- as.character(newdata$STOC_YRX[i])
 STOCKB_ <- as.character(newdata$STOCKB_[i])
 STOCKBX_ <- as.character(newdata$STOCKBX_[i])
 STOCKX_ <- as.character(newdata$STOCKX_[i])
 STUD_INX <- as.character(newdata$STUD_INX[i])
 STUD_TBX <- as.character(newdata$STUD_TBX[i])
 STUDNTB_ <- as.character(newdata$STUDNTB_[i])
 STUDNTX_ <- as.character(newdata$STUDNTX_[i])
 WHLF_RBX <- as.character(newdata$WHLF_RBX[i])
 WHLFYRB_ <- as.character(newdata$WHLFYRB_[i])
 WHLFYRX_ <- as.character(newdata$WHLFYRX_[i])
 WHOL_FBX <- as.character(newdata$WHOL_FBX[i])
 WHOLIFB_ <- as.character(newdata$WHOLIFB_[i])
 WHOLIFX_ <- as.character(newdata$WHOLIFX_[i])
 tmp <- predicted()
 node <- tmp[1]
 rpred <- tmp[2]
 ###gnode <- z$node[i]
 ###gpred <- z$predicted[i]
 ###if(rpred != gpred){
 ###print(c("Case ",i,node,gnode,rpred,gpred))
 ###noerr <- FALSE}
 }
 ###if(noerr == TRUE) print("No errors")
