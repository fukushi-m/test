# changes



library(shiny)
library(dplyr)
#library(diagram)
library(splancs)
library(shinyAce) # Ace editor 
#library(sendmailR)#
library(mail) # sendmail() function can send 20 email per day per ip
#devtools::install_github('rstudio/DT')
library(DT)

library(stringi) # Fukushi
library(stringr) # Fukushi

Sys.setlocale(locale="en_US.UTF-8")
options(scipen = 6)


# ====== Dropbox link ====================================
#library(rdrop2)
#token <- readRDS("droptoken.rds")
#drop_acc(dtoken = token)
#drop_dir(dtoken = token)
#drop_get(path = "clouddoc/ShinylinkedFiles/ScreenTB_releasenotes.htm", overwrite = T,dtoken = token)

#drop_auth()

#drop_dir()

# Functions ==================================================================

substrRight <- function(x,n){substr(x, nchar(x)-n+1, nchar(x))} # added by Fukushi


dominated <- function(poly) {
  require(splancs)
  n <- nrow(poly)
  poly <- rbind(poly,c(max(poly[,1]), min(poly[,2])))
  areamax <- areapl(poly)
  flag  <- rep(T,n)
  i <- 1
  while (i < n+1){
    tempflag <- flag; tempflag[i] <- F
    area <- areapl(poly[tempflag,])
    #    cat("max=",areamax," drop i=",i,"  area=",area,"\n")
    if (areamax < area){
      areamax <- area
      flag<-tempflag
      i <- 1
    }else{
      i <- i+1
    }
  }
  return(!flag)
}

screen <- function(steps,tspec){
  n <- nrow(steps)
  steps$step[n] <- tspec$n
  screened <- steps$TP[n] + steps$FP[n]
  steps$cost[n] <- screened * (tspec$dcost + tspec$ocost)  
#  steps$cost[n] <- screened * (tspec$cost)
  nTP <- steps$TP[n] * tspec$sens
  nFN <- steps$TP[n] * (1-tspec$sens)
  nTN <- steps$FP[n] * tspec$spec
  nFP <- steps$FP[n] * (1-tspec$spec)
  rbind(steps,c(nTP, nFP, nFN, nTN, 0,0))
}

#pop  <- 100000
#prev <- 1000 / 100000
#case <- pop * prev
#alg=c("sym_cgh2w","cxr_abn","xpert","dummy")

acf <- function(pop=100000, prev=1000, algvec="dummy", test){
  algvec=c(algvec,"dummy","dummy","dummy","dummy")[1:4]
  case <- pop * prev/100000
  steps <- data.frame(TP=case, FP=pop-case,FN=0, TN=0, cost=0, step="init",stringsAsFactors=FALSE)
  r <- steps %>%
    screen(test[test$n==algvec[1],]) %>%
    screen(test[test$n==algvec[2],]) %>%
    screen(test[test$n==algvec[3],]) %>%
    screen(test[test$n==algvec[4],]) 
  fin <- c(round(c(TP=r$TP[5],FP=r$FP[5],FN=sum(r$FN),TN=sum(r$TN)),0),cost=sum(r$cost))
  return(fin)
}

# =============================================================================
acfset <- function(pop=100000, prev=1000, alglst,test){
  temp <- as.data.frame(t(as.data.frame(lapply(alglst, function(x) acf(pop,prev,alg=x,test)))))
  temp$cost_tp <- round(temp$cost/temp$TP,0)
  return(temp)
}
# acfset() gives a result of ACF by all algorithms for a single risk group
# acfset(pop=10000,prev=1000/100000,alglst=alg)

# Calcuation of final results (restab) requires followings: 
# test: A dataframe with sensitivity, specifity and cost for each test 
# alg:  A list containing algorithms (diagnostic steps as a vector)
# rgtab: Risk group table (dataframe) with Country, Group, Population, Prevalence


algdf_to_ls <- function(algdf){
  algls <- apply(algdf,1,function(x){body <- x[-1]; body <- body[!body=="dummy"]; names(body) <- NULL; body})
  names(algls) <- algdf[,1]
  algls
}

jointext <- function(x){
  x <- x[!x=="dummy"]
  paste0(x[1],": ",paste(x[-1],collapse=" -> "))
}

updateAllTest <- function(session, values) {
  isolate(updateNumericInput(session,"sen1",value=test[1,"sens"])) 
  isolate(updateNumericInput(session,"spe1",value=test[1,"spec"])) 
  isolate(updateNumericInput(session,"dcost1",value=test[1,"dcost"])) 
  isolate(updateNumericInput(session,"ocost1",value=test[1,"ocost"])) 
  isolate(updateNumericInput(session,"sen2",value=test[2,"sens"])) 
  isolate(updateNumericInput(session,"spe2",value=test[2,"spec"])) 
  isolate(updateNumericInput(session,"dcost2",value=test[2,"dcost"])) 
  isolate(updateNumericInput(session,"ocost2",value=test[2,"ocost"])) 
  isolate(updateNumericInput(session,"sen3",value=test[3,"sens"])) 
  isolate(updateNumericInput(session,"spe3",value=test[3,"spec"])) 
  isolate(updateNumericInput(session,"dcost3",value=test[3,"dcost"])) 
  isolate(updateNumericInput(session,"ocost3",value=test[3,"ocost"])) 
  isolate(updateNumericInput(session,"sen4",value=test[4,"sens"])) 
  isolate(updateNumericInput(session,"spe4",value=test[4,"spec"])) 
  isolate(updateNumericInput(session,"dcost4",value=test[4,"dcost"])) 
  isolate(updateNumericInput(session,"ocost4",value=test[4,"ocost"])) 
  isolate(updateNumericInput(session,"sen8",  value=test[5,"sens"])) 
  isolate(updateNumericInput(session,"spe5",  value=test[5,"spec"])) 
  isolate(updateNumericInput(session,"dcost5",value=test[5,"dcost"])) 
  isolate(updateNumericInput(session,"ocost5",value=test[5,"ocost"])) 
  isolate(updateNumericInput(session,"sen6",  value=test[6,"sens"])) 
  isolate(updateNumericInput(session,"spe6",  value=test[6,"spec"])) 
  isolate(updateNumericInput(session,"dcost6",value=test[6,"dcost"])) 
  isolate(updateNumericInput(session,"ocost6",value=test[6,"ocost"])) 
  isolate(updateNumericInput(session,"sen7",  value=test[7,"sens"])) 
  isolate(updateNumericInput(session,"spe7",  value=test[7,"spec"])) 
  isolate(updateNumericInput(session,"dcost7",value=test[7,"dcost"])) 
  isolate(updateNumericInput(session,"ocost7",value=test[7,"ocost"])) 
  isolate(updateNumericInput(session,"sen8",  value=test[8,"sens"])) 
  isolate(updateNumericInput(session,"spe8",  value=test[8,"spec"])) 
  isolate(updateNumericInput(session,"dcost8",value=test[8,"dcost"])) 
  isolate(updateNumericInput(session,"ocost8",value=test[8,"ocost"])) 
  isolate(updateNumericInput(session,"sen9",  value=test[9,"sens"])) 
  isolate(updateNumericInput(session,"spe9",  value=test[9,"spec"])) 
  isolate(updateNumericInput(session,"dcost9",value=test[9,"dcost"])) 
  isolate(updateNumericInput(session,"ocost9",value=test[9,"ocost"])) 
  isolate(updateNumericInput(session,"sen10",  value=test[10,"sens"])) 
  isolate(updateNumericInput(session,"spe10",  value=test[10,"spec"])) 
  isolate(updateNumericInput(session,"dcost10",value=test[10,"dcost"])) 
  isolate(updateNumericInput(session,"ocost10",value=test[10,"ocost"])) 
  isolate(updateNumericInput(session,"sen11",  value=test[11,"sens"])) 
  isolate(updateNumericInput(session,"spe11",  value=test[11,"spec"])) 
  isolate(updateNumericInput(session,"dcost11",value=test[11,"dcost"])) 
  isolate(updateNumericInput(session,"ocost11",value=test[11,"ocost"]))
  isolate(updateNumericInput(session,"sen12",  value=test[12,"sens"])) 
  isolate(updateNumericInput(session,"spe12",  value=test[12,"spec"])) 
  isolate(updateNumericInput(session,"dcost12",value=test[12,"dcost"])) 
  isolate(updateNumericInput(session,"ocost12",value=test[12,"ocost"]))
  isolate(updateNumericInput(session,"sen13",  value=test[13,"sens"])) 
  isolate(updateNumericInput(session,"spe13",  value=test[13,"spec"])) 
  isolate(updateNumericInput(session,"dcost13",value=test[13,"dcost"])) 
  isolate(updateNumericInput(session,"ocost13",value=test[13,"ocost"]))
  
  isolate(updateTextInput(session,"ctname13",value=test[13,"desc"]))
  isolate(updateTextInput(session,"ctname13",value=names(lstchoi.s1)[11]))
  isolate(updateTextInput(session,"ctname13",value=names(lstchoi.s2)[5]))
  isolate(updateTextInput(session,"ctname13",value=names(lstchoi.d1)[3]))
}



# Loading datasets ==================================================================
#Sys.setlocale(locale="en_US.UTF-8")
load(file="CountryNames.Rdata") # cnames, ls.cnames, ls.isos

#load(file="pop_gtb2012.Rdata")
#load(file="cprev_gtb2000_2013.Rdata")
load(file="cprev_gtb2015.Rdata") # cprev
load(file="pop_gtb2015.Rdata") # pop

#load(file="ACFtool3_shinydata.Rdata")
load(file="rgrouplist.Rdata") # rglist
load(file="testprofile.Rdata") # test

# to replace default with Cambodia 
rglist$pop[1] <- 15328136
rglist$pop[2:6] <- 10000
rglist$iso3 <- "KHM"
rglist$fixpop[2:6] <- TRUE

# change acceptability rate to 100% if no data from systematic review
rglist$accept[rglist$accept==0] <- 100


rgrp.pre <- rglist[1:6,c("rgname","pop","rr","prev","iso3","rgprv","fixprev","fixpop","reach","accept","mtxt_rgprv","mtxt_rr","mtxt_accept")]
rgrp.pre$scpop <- round(rgrp.pre$reach/100 * rgrp.pre$accept/100 * rgrp.pre$pop,0)

# --------- Fukushi
test_dummy <- test[13,] 
test[13,1] <- "Custom1" 
test[13,2] <- 0.00 
test[13,5] <- 0.00 
test[13,8] <- 0.0
test[13,9] <- 0.0 
test[13,10] <- "c1" 
test[13,11] <- "Custom1"
test <- rbind(test, test_dummy)
test[14,1] <- "Custom2" 
test[14,2] <- 0.00 
test[14,5] <- 0.00 
test[14,8] <- 0.0
test[14,9] <- 0.0 
test[14,10] <- "c2" 
test[14,11] <- "Custom2"
test <- rbind(test, test_dummy)
test[15,1] <- "Custom3" 
test[15,2] <- 0.00 
test[15,5] <- 0.00 
test[15,8] <- 0.0
test[15,9] <- 0.0
test[15,10] <- "c3" 
test[15,11] <- "Custom3"
test <- rbind(test, test_dummy)
rownames(test) <- seq(length=nrow(test))
# --------------------- until here

test.pre <- test[,c(1,2,5,8,9,10)]

lstchoi <- as.list(test.pre[,"n"])
names(lstchoi) <- test.pre[,"desc"]
names(lstchoi) <- sub("dummy"," ",names(lstchoi))

lstchoi.s1 <- lstchoi[c(1,2,4:9,10,11,13, 14, 15, 16)] # add 14, 15,16
lstchoi.s2 <- lstchoi[c(1:4,13, 14, 15, 16)] # add 14, 15,16
lstchoi.d1 <- lstchoi[c(10,11,13, 14, 15, 16)] # add 14, 15, 16


algdf.pre <- data.frame(Alg=c("1a","1b","1c","1d","2a","2b","2c","2d","3a","3b","Custom1","Custom2"),
                        Step1=c("sym_cgh2w","sym_cgh2w","sym_cgh2w","sym_cgh2w",
                                "sym_any","sym_any","sym_any","sym_any","cxr_abn","cxr_abn","dummy","dummy"),
                        Step2=c("dummy","dummy","cxr_aftsymp","cxr_aftsymp","dummy","dummy","cxr_aftsymp","cxr_aftsymp","dummy","dummy","dummy","dummy"),
                        Step3=c("ssm","xpert","ssm","xpert","ssm","xpert","ssm","xpert","ssm","xpert","dummy","dummy"),
                        stringsAsFactors = FALSE)

algdf.pre$desc <- apply(algdf.pre,1,jointext)
alg.pre <- algdf_to_ls(algdf.pre[,1:4])



#load(file="Scenario_KHM_2016-04-01.Rdata")
#View(scenariofile$rgrp)
#View(scenariofile$test)
#rgrp.pre <- scenariofile$rgrp
#test.pre <- scenariofile$test
#algdf.pre <- scenariofile$algdf
#alg.pre <- scenariofile$alg
