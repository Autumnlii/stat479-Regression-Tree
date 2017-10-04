newdata <- read.table("cedata5.txt",header=TRUE,colClasses="character")
## Missing value code is NA
## Change file name if needed
## Predicting class
newdata=cedataC
str(newdata)
predicted <- function(){
  catvalues <- c("10", "2", "23", "33")
  if(STATE %in% catvalues){
    catvalues <- c("1", "10", "12", "2", "4", "7")
    if(OCCUCOD2 %in% catvalues){
      nodeid <-  4
      predict <- "4400"
    } else {
      catvalues <- c("12", "15", "16")
      if(EDUCA2 %in% catvalues){
        nodeid <-  10
        predict <- "860"
      } else {
        nodeid <-  11
        predict <- "2700"
      }
    }
  } else {
    catvalues <- c("12", "16", "17", "34", "45", "47", "8")
    if(STATE %in% catvalues){
      catvalues <- c("3", "5", "6")
      if(INCOMEY2 %in% catvalues){
        nodeid <-  12
        predict <- "98338"
      } else {
        catvalues <- c("10", "3", "4", "5")
        if(OCCUCOD2 %in% catvalues){
          nodeid <-  26
          predict <- "50"
        } else {
          catvalues <- c("1111", "1320")
          if(PSU %in% catvalues){
            catvalues <- c("11", "2", "3", "4", "7")
            if(BUILDING %in% catvalues){
              nodeid <-  108
              predict <- "12000"
            } else {
              nodeid <-  109
              predict <- "11601"
            }
          } else {
            catvalues <- c("10", "11", "12", "15", "16")
            if(EDUCA2 %in% catvalues){
              catvalues <- c("17", "34", "47", "8")
              if(STATE %in% catvalues){
                nodeid <-  220
                predict <- "7000"
              } else {
                nodeid <-  221
                predict <- "20000"
              }
            } else {
              catvalues <- c("D")
              if(FEDR_NDX %in% catvalues){
                catvalues <- c("2", "4", "5", "7")
                if(EARNCOMP %in% catvalues){
                  nodeid <-  444
                  predict <- "10"
                } else {
                  if(is.na(FINCBTAX) | is.na(UNISTRQ) |   -1.1456325538799301E+04  * UNISTRQ + FINCBTAX <=   -3.5784649245968576E+03 ){
                    nodeid <-  890
                    predict <- "50"
                  } else {
                    nodeid <-  891
                    predict <- "5"
                  }
                }
              } else {
                if(!is.na(FEDTAXX) & !is.na(RETPENPQ) &  -18.2599480445864302  * RETPENPQ + FEDTAXX <=   -1.8502847368457500E+04 ){
                  catvalues <- c("1", "10", "2", "3", "5", "6", "7")
                  if(OCCUCOD1 %in% catvalues){
                    nodeid <-  892
                    predict <- "150"
                  } else {
                    nodeid <-  893
                    predict <- "1"
                  }
                } else {
                  nodeid <-  447
                  predict <- "1200"
                }
              }
            }
          }
        }
      }
    } else {
      catvalues <- c("11", "13", "15", "20", "21", "25", "26", "27", "31",
                     "32", "36", "39", "42", "49", "53", "55")
      if(STATE %in% catvalues){
        catvalues <- c("13", "15", "25", "26", "27", "31", "53", "55")
        if(STATE %in% catvalues){
          nodeid <-  28
          predict <- "20"
        } else {
          catvalues <- c("4")
          if(EARNCOMP %in% catvalues){
            nodeid <-  58
            predict <- "1"
          } else {
            catvalues <- c("2", "4", "6")
            if(REF_RACE %in% catvalues){
              nodeid <-  118
              predict <- "14000"
            } else {
              catvalues <- c("5", "6", "7", "8")
              if(EARNCOMP %in% catvalues){
                catvalues <- c("1", "10", "11", "12", "15", "2", "7",
                               "8")
                if(OCCUCOD2 %in% catvalues){
                  nodeid <-  476
                  predict <- "1123"
                } else {
                  catvalues <- c("1", "10", "11", "12", "15", "2", "7")
                                 catvalues <- c("20", "21", "32", "36", "39")
                                 if(STATE %in% catvalues){
                                   nodeid <-  954
                                   predict <- "600"
                                 } else {
                                   nodeid <-  955
                                   predict <- "5"
                                 }
                }
              } else {
                catvalues <- c("1", "11", "12", "14", "6", "8")
                if(OCCUCOD1 %in% catvalues){
                  nodeid <-  478
                  predict <- "15"
                } else {
                  nodeid <-  479
                  predict <- "10"
                }
              }
            }
          }
        }
      } else {
        catvalues <- c("11", "13", "15", "20", "21", "25", "26", "27", "31")
                       catvalues <- c("1110", "1312", "1313", "1318", "1424")
                       if(PSU %in% catvalues){
                         nodeid <-  30
                         predict <- "30"
                       } else {
                         catvalues <- c("1", "4", "48", "51", "9")
                         if(STATE %in% catvalues){
                           catvalues <- c("11", "5", "6", "7")
                           if(BUILDING %in% catvalues){
                             catvalues <- c("1", "3", "4", "7", "8")
                             if(OCCUCOD1 %in% catvalues){
                               nodeid <-  248
                               predict <- "200"
                             } else {
                               nodeid <-  249
                               predict <- "98338"
                             }
                           } else {
                             catvalues <- c("1", "10", "15", "2", "3", "4", "9")
                             if(OCCUCOD1 %in% catvalues){
                               catvalues <- c("51", "9")
                               if(STATE %in% catvalues){
                                 nodeid <-  500
                                 predict <- "200"
                               } else {
                                 nodeid <-  501
                                 predict <- "1"
                               }
                             } else {
                               nodeid <-  251
                               predict <- "500"
                             }
                           }
                         } else {
                           catvalues <- c("13", "2", "6")
                           if(OCCUCOD2 %in% catvalues){
                             nodeid <-  126
                             predict <- "500"
                           } else {
                             nodeid <-  127
                             predict <- "200"
                           }
                         }
                       }
      }
    }
  }
  return(c(nodeid,predict))
}
###z <- read.table("fit2.txt",header=TRUE)
###noerr <- TRUE
dim(newdata)[1]
for(i in 1:dim(newdata)[1]){
  DIRACC <- as.character(newdata$DIRACC[i])
  DIRACC_ <- as.character(newdata$DIRACC_[i])
  AGE_REF_ <- as.character(newdata$AGE_REF_[i])
  AGE2_ <- as.character(newdata$AGE2_[i])
  AS_C_MP1 <- as.character(newdata$AS_C_MP1[i])
  AS_C_MP2 <- as.character(newdata$AS_C_MP2[i])
  AS_C_MP3 <- as.character(newdata$AS_C_MP3[i])
  AS_C_MP4 <- as.character(newdata$AS_C_MP4[i])
  AS_C_MP5 <- as.character(newdata$AS_C_MP5[i])
  BATHRMQ_ <- as.character(newdata$BATHRMQ_[i])
  BEDR_OMQ <- as.character(newdata$BEDR_OMQ[i])
  BUILDING <- as.character(newdata$BUILDING[i])
  BUIL_ING <- as.character(newdata$BUIL_ING[i])
  CUTENURE <- as.character(newdata$CUTENURE[i])
  CUTE_URE <- as.character(newdata$CUTE_URE[i])
  EARNCOMP <- as.character(newdata$EARNCOMP[i])
  EARN_OMP <- as.character(newdata$EARN_OMP[i])
  EDUC0REF <- as.character(newdata$EDUC0REF[i])
  EDUCA2 <- as.character(newdata$EDUCA2[i])
  EDUCA2_ <- as.character(newdata$EDUCA2_[i])
  FAM__IZE <- as.character(newdata$FAM__IZE[i])
  FAM_TYPE <- as.character(newdata$FAM_TYPE[i])
  FAM__YPE <- as.character(newdata$FAM__YPE[i])
  FAMT_EDX <- as.character(newdata$FAMT_EDX[i])
  FEDR_NDX <- as.character(newdata$FEDR_NDX[i])
  FEDTAXX <- as.numeric(newdata$FEDTAXX[i])
  FEDTAXX_ <- as.character(newdata$FEDTAXX_[i])
  FGOV_ETX <- as.character(newdata$FGOV_ETX[i])
  FINCAT_X <- as.character(newdata$FINCAT_X[i])
  FINCBTAX <- as.numeric(newdata$FINCBTAX[i])
  FINCBT_X <- as.character(newdata$FINCBT_X[i])
  FIND_ETX <- as.character(newdata$FIND_ETX[i])
  FJSS_EDX <- as.character(newdata$FJSS_EDX[i])
  FPRI_ENX <- as.character(newdata$FPRI_ENX[i])
  FRRDEDX_ <- as.character(newdata$FRRDEDX_[i])
  FRRE_IRX <- as.character(newdata$FRRE_IRX[i])
  FSAL_RYX <- as.character(newdata$FSAL_RYX[i])
  FSLTAXX_ <- as.character(newdata$FSLTAXX_[i])
  FSSIX_ <- as.character(newdata$FSSIX_[i])
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
  INCW_EK1 <- as.character(newdata$INCW_EK1[i])
  INCW_EK2 <- as.character(newdata$INCW_EK2[i])
  MISC_AXX <- as.character(newdata$MISC_AXX[i])
  LUMP_UMX <- as.character(newdata$LUMP_UMX[i])
  MARITAL1 <- as.character(newdata$MARITAL1[i])
  MARI_AL1 <- as.character(newdata$MARI_AL1[i])
  NO_E_RNR <- as.character(newdata$NO_E_RNR[i])
  NONI_CMX <- as.character(newdata$NONI_CMX[i])
  NUM__UTO <- as.character(newdata$NUM__UTO[i])
  OCCUCOD1 <- as.character(newdata$OCCUCOD1[i])
  OCCU_OD1 <- as.character(newdata$OCCU_OD1[i])
  OCCUCOD2 <- as.character(newdata$OCCUCOD2[i])
  OCCU_OD2 <- as.character(newdata$OCCU_OD2[i])
  OTHR_NCX <- as.character(newdata$OTHR_NCX[i])
  PERS_T18 <- as.character(newdata$PERS_T18[i])
  PERS_T64 <- as.character(newdata$PERS_T64[i])
  PRINEARN <- as.character(newdata$PRINEARN[i])
  PRIN_ARN <- as.character(newdata$PRIN_ARN[i])
  QINTRVMO <- as.character(newdata$QINTRVMO[i])
  QINTRVYR <- as.character(newdata$QINTRVYR[i])
  RACE2 <- as.character(newdata$RACE2[i])
  RACE2_ <- as.character(newdata$RACE2_[i])
  REF_RACE <- as.character(newdata$REF_RACE[i])
  REF__ACE <- as.character(newdata$REF__ACE[i])
  REGION <- as.character(newdata$REGION[i])
  RENT_QVX <- as.character(newdata$RENT_QVX[i])
  RESPSTAT <- as.character(newdata$RESPSTAT[i])
  RESP_TAT <- as.character(newdata$RESP_TAT[i])
  ROOMSQ_ <- as.character(newdata$ROOMSQ_[i])
  SEX_REF <- as.character(newdata$SEX_REF[i])
  SEX_REF_ <- as.character(newdata$SEX_REF_[i])
  SEX2 <- as.character(newdata$SEX2[i])
  SEX2_ <- as.character(newdata$SEX2_[i])
  SLOC_AXX <- as.character(newdata$SLOC_AXX[i])
  SLRF_NDX <- as.character(newdata$SLRF_NDX[i])
  SMSASTAT <- as.character(newdata$SMSASTAT[i])
  ST_HOUS <- as.character(newdata$ST_HOUS[i])
  ST_HOUS_ <- as.character(newdata$ST_HOUS_[i])
  TOTT_PDX <- as.character(newdata$TOTT_PDX[i])
  VEHQ_ <- as.character(newdata$VEHQ_[i])
  WELF_REX <- as.character(newdata$WELF_REX[i])
  RETPENPQ <- as.numeric(newdata$RETPENPQ[i])
  HH_CU_Q_ <- as.character(newdata$HH_CU_Q_[i])
  HHID <- as.character(newdata$HHID[i])
  HHID_ <- as.character(newdata$HHID_[i])
  POV_CY <- as.character(newdata$POV_CY[i])
  POV_CY_ <- as.character(newdata$POV_CY_[i])
  POV_PY <- as.character(newdata$POV_PY[i])
  POV_PY_ <- as.character(newdata$POV_PY_[i])
  SWIMPOOL <- as.character(newdata$SWIMPOOL[i])
  SWIM_OOL <- as.character(newdata$SWIM_OOL[i])
  APTMENT <- as.character(newdata$APTMENT[i])
  APTMENT_ <- as.character(newdata$APTMENT_[i])
  OFSTPARK <- as.character(newdata$OFSTPARK[i])
  OFST_ARK <- as.character(newdata$OFST_ARK[i])
  WINDOWAC <- as.character(newdata$WINDOWAC[i])
  WIND_WAC <- as.character(newdata$WIND_WAC[i])
  CNTRALAC <- as.character(newdata$CNTRALAC[i])
  CNTR_LAC <- as.character(newdata$CNTR_LAC[i])
  CHILDAGE <- as.character(newdata$CHILDAGE[i])
  CHIL_AGE <- as.character(newdata$CHIL_AGE[i])
  STATE <- as.character(newdata$STATE[i])
  ERANKH_ <- as.character(newdata$ERANKH_[i])
  VEHQL_ <- as.character(newdata$VEHQL_[i])
  NUM__VAN <- as.character(newdata$NUM__VAN[i])
  POVL_VCY <- as.character(newdata$POVL_VCY[i])
  POVL_VPY <- as.character(newdata$POVL_VPY[i])
  PORCH <- as.character(newdata$PORCH[i])
  PORCH_ <- as.character(newdata$PORCH_[i])
  UNISTRQ <- as.numeric(newdata$UNISTRQ[i])
  UNISTRQ_ <- as.character(newdata$UNISTRQ_[i])
  WELF_EBX <- as.character(newdata$WELF_EBX[i])
  LUMP_UMB <- as.character(newdata$LUMP_UMB[i])
  LMPS_MBX <- as.character(newdata$LMPS_MBX[i])
  OTHR_NCB <- as.character(newdata$OTHR_NCB[i])
  OTRI_CBX <- as.character(newdata$OTRI_CBX[i])
  INCL_SS2 <- as.character(newdata$INCL_SS2[i])
  INTERI <- as.character(newdata$INTERI[i])
  HORREF1 <- as.character(newdata$HORREF1[i])
  HORREF1_ <- as.character(newdata$HORREF1_[i])
  HORREF2 <- as.character(newdata$HORREF2[i])
  HORREF2_ <- as.character(newdata$HORREF2_[i])
  ERANKHM_ <- as.character(newdata$ERANKHM_[i])
  FGOV_ETM <- as.character(newdata$FGOV_ETM[i])
  FPRI_ENM <- as.character(newdata$FPRI_ENM[i])
  FRRDEDM_ <- as.character(newdata$FRRDEDM_[i])
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
  FFTA_OWE <- as.character(newdata$FFTA_OWE[i])
  FMLP_YRX <- as.character(newdata$FMLP_YRX[i])
  FS_MTHI_ <- as.character(newdata$FS_MTHI_[i])
  FSMP_RMX <- as.character(newdata$FSMP_RMX[i])
  FSTA_OWE <- as.character(newdata$FSTA_OWE[i])
  INTR_VBX <- as.character(newdata$INTR_VBX[i])
  INTRDVB <- as.character(newdata$INTRDVB[i])
  INTRDVB_ <- as.character(newdata$INTRDVB_[i])
  INTRDVX_ <- as.character(newdata$INTRDVX_[i])
  IRAB_ <- as.character(newdata$IRAB_[i])
  IRABX_ <- as.character(newdata$IRABX_[i])
  IRAX_ <- as.character(newdata$IRAX_[i])
  IRAYRB_ <- as.character(newdata$IRAYRB_[i])
  IRAYRBX_ <- as.character(newdata$IRAYRBX_[i])
  IRAYRX_ <- as.character(newdata$IRAYRX_[i])
  JFS_AMT_ <- as.character(newdata$JFS_AMT_[i])
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
  RETSURV_ <- as.character(newdata$RETSURV_[i])
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
  node[i] <- tmp[1]
  rpred[i] <- tmp[2]
  ###gnode <- z$node[i]
  ###gpred <- z$predicted[i]
  ###if(rpred != gpred){
  ###print(c("Case ",i,node,gnode,rpred,gpred))
  ###noerr <- FALSE}
}
###if(noerr == TRUE) print("No errors")
