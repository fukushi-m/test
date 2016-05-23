shinyServer(function(input, output, session) {

  values <- reactiveValues() # values <- NA , then run below four lines to see its contents
  values$rgrp <- rgrp.pre 
  values$test <- test.pre 
  values$algdf <- algdf.pre 
  values$alg <- alg.pre 
  
  cdata <- session$clientData
  
  ########## addtion by Fukushi ############ 

  # observe({
#    if(input$ctname13!="Custom1") {    
#      values$test[13, "desc"] <- input$ctname13
#      names(lstchoi.s1)[11] <- input$ctname13 # Fukushi
#      names(lstchoi.s2)[5] <- input$ctname13 # Fukushi
#      names(lstchoi.d1)[3] <- input$ctname13# Fukushi   
#    }
#  })

  observe({
    values$test[13, "desc"] <- input$ctname13
    names(lstchoi.s1)[11] <- input$ctname13 
    names(lstchoi.s2)[5] <- input$ctname13 
    names(lstchoi.d1)[3] <- input$ctname13
    
    values$test[14, "desc"] <- input$ctname14
    names(lstchoi.s1)[12] <- input$ctname14 
    names(lstchoi.s2)[6] <- input$ctname14 
    names(lstchoi.d1)[4] <- input$ctname14
    
    values$test[15, "desc"] <- input$ctname15
    names(lstchoi.s1)[13] <- input$ctname15
    names(lstchoi.s2)[7] <- input$ctname15
    names(lstchoi.d1)[5] <- input$ctname15
  })
  
  observe({
    if (input$fxp1==TRUE) {
      shinyjs::disable("rr1")
      shinyjs::enable("prv1")
    } else {
      shinyjs::enable("rr1")
      shinyjs::disable("prv1")
    }
  })
  
  observe({
    if (input$fxp2==TRUE) {
      shinyjs::disable("rr2")
      shinyjs::enable("prv2")
    } else {
      shinyjs::enable("rr2")
      shinyjs::disable("prv2")
    }
  })  
  observe({
    if (input$fxp3==TRUE) {
      shinyjs::disable("rr3")
      shinyjs::enable("prv3")
    } else {
      shinyjs::enable("rr3")
      shinyjs::disable("prv3")
    }
  })  
  observe({
    if (input$fxp4==TRUE) {
      shinyjs::disable("rr4")
      shinyjs::enable("prv4")
    } else {
      shinyjs::enable("rr4")
      shinyjs::disable("prv4")
    }
  })  
  observe({
    if (input$fxp5==TRUE) {
      shinyjs::disable("rr5")
      shinyjs::enable("prv5")
    } else {
      shinyjs::enable("rr5")
      shinyjs::disable("prv5")
    }
  })  
  observe({
    if (input$fxp6==TRUE) {
      shinyjs::disable("rr6")
      shinyjs::enable("prv6")
    } else {
      shinyjs::enable("rr6")
      shinyjs::disable("prv6")
    }
  })  
  
  # addition by Fukushi
  observe({
    if (input$fxpop1==TRUE) {
      shinyjs::disable("rgprv1")
      shinyjs::enable("pop1")
    } else {
      shinyjs::enable("rgprv1")
      shinyjs::disable("pop1")
    }
  })
  observe({
    if (input$fxpop2==TRUE) {
      shinyjs::disable("rgprv2")
      shinyjs::enable("pop2")
    } else {
      shinyjs::enable("rgprv2")
      shinyjs::disable("pop2")
    }
  })
  observe({
    if (input$fxpop3==TRUE) {
      shinyjs::disable("rgprv3")
      shinyjs::enable("pop3")
    } else {
      shinyjs::enable("rgprv3")
      shinyjs::disable("pop3")
    }
  })
  observe({
    if (input$fxpop4==TRUE) {
      shinyjs::disable("rgprv4")
      shinyjs::enable("pop4")
    } else {
      shinyjs::enable("rgprv4")
      shinyjs::disable("pop4")
    }
  })
  observe({
    if (input$fxpop5==TRUE) {
      shinyjs::disable("rgprv5")
      shinyjs::enable("pop5")
    } else {
      shinyjs::enable("rgprv5")
      shinyjs::disable("pop5")
    }
  })
  observe({
    if (input$fxpop6==TRUE) {
      shinyjs::disable("rgprv6")
      shinyjs::enable("pop6")
    } else {
      shinyjs::enable("rgprv6")
      shinyjs::disable("pop6")
    }
  })
  # further addition by FM
  
  observe({
    if (input$baseprevmethod==TRUE) {
      shinyjs::disable("baseprev_i")
    } else {
      shinyjs::enable("baseprev_i")
    }
  })
  
  #------------------------
  
  
  
  
  observe({
    if(is.null(input$send) || input$send==0) return(NULL)
    from <- isolate(input$from)
    name <- isolate(input$name)
#    subject <- isolate(input$subject)
    msg <- isolate(input$message)
    sendmail("nobu.nishikiori@gmail.com", "ScreenTB feedback", 
             paste0("From: ",name,"  ",from,"\n\n",msg))
    isolate(updateAceEditor(session, "message", value="Your message has been sent!"))
#-----------------------
#    sendmail(from, to, subject, msg, control = list(smtpServer="ASPMX.L.GOOGLE.COM")) 
#     sendmail(from, to, subject, msg,
#              smtp = list(host.name = "smtp.gmail.com", port = 465, 
#                          user.name = "nobu.in.japan", passwd = "jccn1002", 
#                          ssl = TRUE, tls = TRUE),
#              authenticate = TRUE,
#              send = TRUE)
#----------------------
  })

  obs_bprev <- observe({
    values$rgrp[,"iso3"] <- input$baseiso
    if(input$baseprevmethod){
      values$bprev <- cprev %>% 
      filter(iso3==input$baseiso, year==input$baseyear) %>% select(e_prev_100k)
    }else{
      values$bprev <- input$baseprev_i
    }
  })

  obs_cpop <- observe({
    values$cpop <- p %>% 
      filter(iso3==input$baseiso, year==input$baseyear) %>% select(e_pop_num)
  })

  obs_rglookup1 <- observe({
    if(input$rglookup1=="---"){return()}else{
      rgnum <- match(input$rglookup1,rglist$rgname)
      isolate(updateTextInput(session,"gname1", value=rglist[rgnum,"rgname"]))
      isolate(updateNumericInput(session,"pop1", value=rglist[rgnum,"pop"]))
      isolate(updateNumericInput(session,"rr1", value=rglist[rgnum,"rr"]))
      isolate(updateNumericInput(session,"prv1", value=rglist[rgnum,"prev"]))
      isolate(updateNumericInput(session,"rgprv1", value=rglist[rgnum,"rgprv"]))
      isolate(updateCheckboxInput(session,"fxp1",value=rglist[rgnum,"fixprev"]))
      isolate(updateCheckboxInput(session,"fxpop1",value=rglist[rgnum,"fixpop"]))
      isolate(updateNumericInput(session,"accept1",value=rglist[rgnum,"accept"]))
      isolate(values$rgrp[1,"mtxt_rr"] <- rglist$mtxt_rr[rgnum]) # FM
      isolate(values$rgrp[1,"mtxt_accept"] <- rglist$mtxt_accept[rgnum]) # FM
    }
  })
  obs_rglookup2 <- observe({
    if(input$rglookup2=="---"){return()}else{
      rgnum <- match(input$rglookup2,rglist$rgname)
      isolate(updateTextInput(session,"gname2", value=rglist[rgnum,"rgname"]))
      isolate(updateNumericInput(session,"pop2", value=rglist[rgnum,"pop"]))
      isolate(updateNumericInput(session,"rr2", value=rglist[rgnum,"rr"]))
      isolate(updateNumericInput(session,"prv2", value=rglist[rgnum,"prev"]))
      isolate(updateNumericInput(session,"rgprv2", value=rglist[rgnum,"rgprv"]))
      isolate(updateCheckboxInput(session,"fxp2",value=rglist[rgnum,"fixprev"]))
      isolate(updateCheckboxInput(session,"fxpop2",value=rglist[rgnum,"fixpop"]))
      isolate(updateNumericInput(session,"accept2",value=rglist[rgnum,"accept"]))
      isolate(values$rgrp[2,"mtxt_rr"] <- rglist$mtxt_rr[rgnum]) # FM
      isolate(values$rgrp[2,"mtxt_accept"] <- rglist$mtxt_accept[rgnum]) # FM
    }
  })
  obs_rglookup3 <- observe({
    if(input$rglookup3=="---"){return()}else{
    rgnum <- match(input$rglookup3,rglist$rgname)
    isolate(updateTextInput(session,"gname3", value=rglist[rgnum,"rgname"]))
    isolate(updateNumericInput(session,"pop3", value=rglist[rgnum,"pop"]))
    isolate(updateNumericInput(session,"rr3", value=rglist[rgnum,"rr"]))
    isolate(updateNumericInput(session,"prv3", value=rglist[rgnum,"prev"]))
    isolate(updateNumericInput(session,"rgprv3", value=rglist[rgnum,"rgprv"]))
    isolate(updateCheckboxInput(session,"fxp3",value=rglist[rgnum,"fixprev"]))
    isolate(updateCheckboxInput(session,"fxpop3",value=rglist[rgnum,"fixpop"]))
    isolate(updateNumericInput(session,"accept3",value=rglist[rgnum,"accept"]))
    isolate(values$rgrp[3,"mtxt_rr"] <- rglist$mtxt_rr[rgnum]) # FM
    isolate(values$rgrp[3,"mtxt_accept"] <- rglist$mtxt_accept[rgnum]) # FM
    }
  })
  obs_rglookup4 <- observe({
    if(input$rglookup4=="---"){return()}else{
    rgnum <- match(input$rglookup4,rglist$rgname)
    isolate(updateTextInput(session,"gname4", value=rglist[rgnum,"rgname"]))
    isolate(updateNumericInput(session,"pop4", value=rglist[rgnum,"pop"]))
    isolate(updateNumericInput(session,"rr4", value=rglist[rgnum,"rr"]))
    isolate(updateNumericInput(session,"prv4", value=rglist[rgnum,"prev"]))
    isolate(updateNumericInput(session,"rgprv4", value=rglist[rgnum,"rgprv"]))
    isolate(updateCheckboxInput(session,"fxp4",value=rglist[rgnum,"fixprev"]))
    isolate(updateCheckboxInput(session,"fxpop4",value=rglist[rgnum,"fixpop"]))
    isolate(updateNumericInput(session,"accept4",value=rglist[rgnum,"accept"]))
    isolate(values$rgrp[4,"mtxt_rr"] <- rglist$mtxt_rr[rgnum]) # FM
    isolate(values$rgrp[4,"mtxt_accept"] <- rglist$mtxt_accept[rgnum]) # FM
    }
  })
  obs_rglookup5 <- observe({
    if(input$rglookup5=="---"){return()}else{
    rgnum <- match(input$rglookup5,rglist$rgname)
    isolate(updateTextInput(session,"gname5", value=rglist[rgnum,"rgname"]))
    isolate(updateNumericInput(session,"pop5", value=rglist[rgnum,"pop"]))
    isolate(updateNumericInput(session,"rr5", value=rglist[rgnum,"rr"]))
    isolate(updateNumericInput(session,"prv5", value=rglist[rgnum,"prev"]))
    isolate(updateNumericInput(session,"rgprv5", value=rglist[rgnum,"rgprv"]))
    isolate(updateCheckboxInput(session,"fxp5",value=rglist[rgnum,"fixprev"]))
    isolate(updateCheckboxInput(session,"fxpop5",value=rglist[rgnum,"fixpop"]))
    isolate(updateNumericInput(session,"accept5",value=rglist[rgnum,"accept"]))
    isolate(values$rgrp[5,"mtxt_rr"] <- rglist$mtxt_rr[rgnum]) # FM
    isolate(values$rgrp[5,"mtxt_accept"] <- rglist$mtxt_accept[rgnum]) # FM
    }
  })
  obs_rglookup6 <- observe({
    if(input$rglookup6=="---"){return()}else{
    rgnum <- match(input$rglookup6,rglist$rgname)
    isolate(updateTextInput(session,"gname6", value=rglist[rgnum,"rgname"]))
    isolate(updateNumericInput(session,"pop6", value=rglist[rgnum,"pop"]))
    isolate(updateNumericInput(session,"rr6", value=rglist[rgnum,"rr"]))
    isolate(updateNumericInput(session,"prv6", value=rglist[rgnum,"prev"]))
    isolate(updateNumericInput(session,"rgprv6", value=rglist[rgnum,"rgprv"]))
    isolate(updateCheckboxInput(session,"fxp6",value=rglist[rgnum,"fixprev"]))
    isolate(updateCheckboxInput(session,"fxpop6",value=rglist[rgnum,"fixpop"]))
    isolate(updateNumericInput(session,"accept6",value=rglist[rgnum,"accept"]))
    isolate(values$rgrp[6,"mtxt_rr"] <- rglist$mtxt_rr[rgnum]) # FM
    isolate(values$rgrp[6,"mtxt_accept"] <- rglist$mtxt_accept[rgnum]) # FM
    }
  })
  
  newEntry <- observe({
      values$rgrp[,"rgname"] <- c(input$gname1,input$gname2,input$gname3,input$gname4,input$gname5,input$gname6)
      values$rgrp[,"fixprev"] <- c(input$fxp1,input$fxp2,input$fxp3,input$fxp4,input$fxp5,input$fxp6)
      values$test[13, "desc"] <- input$ctname13 # Fukushi
      values$test[14, "desc"] <- input$ctname14 # Fukushi
      values$test[15, "desc"] <- input$ctname15 # Fukushi
      
      if(input$fxp1){
        input$rr1
        values$rgrp[1,"prev"] <- input$prv1
        values$rgrp[1,"rr"] <- round(input$prv1/as.numeric(values$bprev),2)
        #isolate(updateNumericInput(session,"prv1", value=values$rgrp[1,"prev"]))
      }else{
        input$prv1
        values$rgrp[1,"rr"] <- input$rr1
        values$rgrp[1,"prev"] <- round(input$rr1*as.numeric(values$bprev),0)
        #isolate(updateNumericInput(session,"rr1", value=values$rgrp[1,"rr"]))
      }
      #updateNumericInput(session,"prv1", value=values$rgrp[1,"prev"]) # Fukushi
      #updateNumericInput(session,"rr1", value=values$rgrp[1,"rr"]) # Fukushi
      
      if(input$fxp2){
        input$rr2
        values$rgrp[2,"prev"] <- input$prv2
        values$rgrp[2,"rr"] <- input$prv2/as.numeric(values$bprev)
      }else{
        input$prv2
        values$rgrp[2,"rr"] <- input$rr2
        values$rgrp[2,"prev"] <- input$rr2*as.numeric(values$bprev)
      }
      #updateNumericInput(session,"prv2", value=values$rgrp[2,"prev"]) # Fukushi
      #updateNumericInput(session,"rr2", value=values$rgrp[2,"rr"]) # Fukushi
      
      if(input$fxp3){
        input$rr3
        values$rgrp[3,"prev"] <- input$prv3
        values$rgrp[3,"rr"] <- input$prv3/as.numeric(values$bprev)
      }else{
        input$prv3
        values$rgrp[3,"rr"] <- input$rr3
        values$rgrp[3,"prev"] <- input$rr3*as.numeric(values$bprev)
      }
      #updateNumericInput(session,"prv3", value=values$rgrp[3,"prev"]) # Fukushi
      #updateNumericInput(session,"rr3", value=values$rgrp[3,"rr"]) # Fukushi
      
      if(input$fxp4){
        input$rr4
        values$rgrp[4,"prev"] <- input$prv4
        values$rgrp[4,"rr"] <- input$prv4/as.numeric(values$bprev)
      }else{
        input$prv4
        values$rgrp[4,"rr"] <- input$rr4
        values$rgrp[4,"prev"] <- input$rr4*as.numeric(values$bprev)
      }
      #updateNumericInput(session,"prv4", value=values$rgrp[4,"prev"]) # Fukushi
      #updateNumericInput(session,"rr4", value=values$rgrp[4,"rr"]) # Fukushi
      
      if(input$fxp5){
        input$rr5
        values$rgrp[5,"prev"] <- input$prv5
        values$rgrp[5,"rr"] <- input$prv5/as.numeric(values$bprev)
      }else{
        input$prv5
        values$rgrp[5,"rr"] <- input$rr5
        values$rgrp[5,"prev"] <- input$rr5*as.numeric(values$bprev)
      }
      #updateNumericInput(session,"prv5", value=values$rgrp[5,"prev"]) # Fukushi
      #updateNumericInput(session,"rr5", value=values$rgrp[5,"rr"]) # Fukushi
      
      if(input$fxp6){
        input$rr6
        values$rgrp[6,"prev"] <- input$prv6
        values$rgrp[6,"rr"] <- input$prv6/as.numeric(values$bprev)
      }else{
        input$prv6
        values$rgrp[6,"rr"] <- input$rr6
        values$rgrp[6,"prev"] <- input$rr6*as.numeric(values$bprev)
      }
      #updateNumericInput(session,"prv6", value=values$rgrp[6,"prev"]) # Fukushi
      #updateNumericInput(session,"rr6", value=values$rgrp[6,"rr"]) # Fukushi
  })
  
  newRgpop <- observe({      
      values$rgrp[,"fixpop"] <- c(input$fxpop1,input$fxpop2,input$fxpop3,input$fxpop4,input$fxpop5,input$fxpop6)


      if(input$fxpop1){
        input$rgprv1
        values$rgrp[1,"pop"]   <- input$pop1
        values$rgrp[1,"rgprv"] <- input$pop1/as.numeric(values$cpop)*100
        #isolate(updateNumericInput(session,"rgprv1", value=values$rgrp[1,"rgprv"]))
      }else{
        input$pop1
        values$rgrp[1,"rgprv"] <- input$rgprv1
        values$rgrp[1,"pop"]   <- round(input$rgprv1*as.numeric(values$cpop)/100,0)
        #isolate(updateNumericInput(session,"pop1", value=values$rgrp[1,"pop"]))
      }
      
      if(input$fxpop2){
        input$rgprv2
        values$rgrp[2,"pop"]   <- input$pop2
        values$rgrp[2,"rgprv"] <- input$pop2/as.numeric(values$cpop)*100
       # isolate(updateNumericInput(session,"rgprv2", value=values$rgrp[2,"rgprv"]))
      }else{
        input$pop2
        values$rgrp[2,"rgprv"] <- input$rgprv2
        values$rgrp[2,"pop"]   <- round(input$rgprv2*as.numeric(values$cpop)/100,0)
       # isolate(updateNumericInput(session,"pop2", value=values$rgrp[2,"pop"]))
      }
      
      if(input$fxpop3){
        input$rgprv3
        values$rgrp[3,"pop"]   <- input$pop3
        values$rgrp[3,"rgprv"] <- input$pop3/as.numeric(values$cpop)*100
        #isolate(updateNumericInput(session,"rgprv3", value=values$rgrp[3,"rgprv"]))
      }else{
        input$pop3
        values$rgrp[3,"rgprv"] <- input$rgprv3
        values$rgrp[3,"pop"]   <- round(input$rgprv3*as.numeric(values$cpop)/100,0)
        #isolate(updateNumericInput(session,"pop3", value=values$rgrp[3,"pop"]))
      }
      
      if(input$fxpop4){
        input$rgprv4
        values$rgrp[4,"pop"]   <- input$pop4
        values$rgrp[4,"rgprv"] <- input$pop4/as.numeric(values$cpop)*100
        #isolate(updateNumericInput(session,"rgprv4", value=values$rgrp[4,"rgprv"]))
      }else{
        #input$pop4
        values$rgrp[4,"rgprv"] <- input$rgprv4
        values$rgrp[4,"pop"]   <- round(input$rgprv4*as.numeric(values$cpop)/100,0)
        #isolate(updateNumericInput(session,"pop4", value=values$rgrp[4,"pop"]))
      }
      
      if(input$fxpop5){
        input$rgprv5
        values$rgrp[5,"pop"]   <- input$pop5
        values$rgrp[5,"rgprv"] <- input$pop5/as.numeric(values$cpop)*100
        #isolate(updateNumericInput(session,"rgprv5", value=values$rgrp[5,"rgprv"]))
      }else{
        input$pop5
        values$rgrp[5,"rgprv"] <- input$rgprv5
        values$rgrp[5,"pop"]   <- round(input$rgprv5*as.numeric(values$cpop)/100,0)
        #isolate(updateNumericInput(session,"pop5", value=values$rgrp[5,"pop"]))
      }
      
      if(input$fxpop6){
        input$rgprv6
        values$rgrp[6,"pop"]   <- input$pop6
        values$rgrp[6,"rgprv"] <- input$pop6/as.numeric(values$cpop)*100
        #isolate(updateNumericInput(session,"rgprv6", value=values$rgrp[6,"rgprv"]))
      }else{
        input$pop6
        values$rgrp[6,"rgprv"] <- input$rgprv6
        values$rgrp[6,"pop"]   <- round(input$rgprv6*as.numeric(values$cpop)/100,0)
        #isolate(updateNumericInput(session,"pop6", value=values$rgrp[6,"pop"]))
      }
  })
  
obs_scpop <- observe({
  allreach  <- c(input$reach1,input$reach2,input$reach3,input$reach4,input$reach5,input$reach6)
  allaccept <- c(input$accept1,input$accept2,input$accept3,input$accept4,input$accept5,input$accept6)
  values$rgrp[,"reach"]  <- allreach
  values$rgrp[,"accept"] <- allaccept
  values$rgrp[,"scpop"] <- round(allreach/100 * allaccept/100 * values$rgrp[,"pop"],0)
})

obs_algdf <- observe({
  values$algdf[,"Alg"] <- gsub(" ","_",
                               c(input$algname1u,input$algname2u,input$algname3u,input$algname4u,input$algname5u,input$algname6u,input$algname7u,input$algname8u,input$algname9u,input$algname10u,input$algname11u,input$algname12u))
  
  s1test_all  <- c(input$s1test1,input$s1test2,input$s1test3,input$s1test4,input$s1test5,input$s1test6,input$s1test7,input$s1test8,input$s1test9,input$s1test10,input$s1test11,input$s1test12,input$s1test13)
  s2test_all  <- c(input$s2test1,input$s2test2,input$s2test3,input$s2test4,input$s2test5,input$s2test6,input$s2test7,input$s2test8,input$s2test9,input$s2test10,input$s2test11,input$s2test12, input$s2test13)
  d1test_all  <- c(input$d1test1,input$d1test2,input$d1test3,input$d1test4,input$d1test5,input$d1test6,input$d1test7,input$d1test8,input$d1test9,input$d1test10,input$d1test11,input$d1test12, input$d1test13)
  values$algdf[,"Step1"]  <- s1test_all
  values$algdf[,"Step2"]  <- s2test_all
  values$algdf[,"Step3"]  <- d1test_all
  values$alg <- algdf_to_ls(values$algdf[,1:4])
  values$algdf[,"desc"]   <- apply(values$algdf[,1:4],1,jointext)
})

  obs_defgrp <- observe({
    if(input$defgrp > 0) {
      values$rgrp <- rgrp.pre
      isolate(updateSelectizeInput(session,"rglookup1",selected=values$rgrp[1,"rgname"])) 
      isolate(updateSelectizeInput(session,"rglookup2",selected=values$rgrp[2,"rgname"])) 
      isolate(updateSelectizeInput(session,"rglookup3",selected=values$rgrp[3,"rgname"])) 
      isolate(updateSelectizeInput(session,"rglookup4",selected=values$rgrp[4,"rgname"])) 
      isolate(updateSelectizeInput(session,"rglookup5",selected=values$rgrp[5,"rgname"])) 
      isolate(updateSelectizeInput(session,"rglookup6",selected=values$rgrp[6,"rgname"])) 
    }
  })

# change - 12 to 15: Fukushi by adding three custom tests
  testUpdate <- observe({
      values$test[1:15,"sens"] <- c(input$sen1,input$sen2,input$sen3,input$sen4,input$sen5,input$sen6,
                                    input$sen7,input$sen8,input$sen9,input$sen10,input$sen11,input$sen12,input$sen13, input$sen14, input$sen15)
      values$test[1:15,"spec"] <- c(input$spe1,input$spe2,input$spe3,input$spe4,input$spe5,input$spe6,
                                    input$spe7,input$spe8,input$spe9,input$spe10,input$spe11,input$spe12, input$spe13, input$spe14, input$spe15)
      values$test[1:15,"dcost"] <- c(input$dcost1,input$dcost2,input$dcost3,input$dcost4,input$dcost5,input$dcost6,
                                     input$dcost7,input$dcost8,input$dcost9,input$dcost10,input$dcost11,input$dcost12, input$dcost13, input$dcost14, input$dcost15)
      values$test[1:15,"ocost"] <- c(input$ocost1,input$ocost2,input$ocost3,input$ocost4,input$ocost5,input$ocost6,
                                     input$ocost7,input$ocost8,input$ocost9,input$ocost10,input$ocost11,input$ocost12,input$ocost13, input$ocost14, input$ocost15)
      
  })
  
#obs_customtest <- observe({})


  obs_process <- observe({
    values$dummyresult <- cbind(values$rgrp,values$rgrp[,2:4]*10)
    isolate(names(values$dummyresult) <- c(names(values$rgrp),"Pop2","RR2","Prev2"))
    values$dummyresult
  })

  obs_screen <- observe({
    restab <- NULL
    rgtab <- values$rgrp
#    alg  <- ifelse(is.null(values$alg), alg.pre, values$alg)
    alg <- values$alg # for debug
    test <- values$test
#    test <- ifelse(is.null(values$test),test.pre,values$test)
    for(i in 1:nrow(rgtab)){
#      gname <- paste(rgtab[i,"Country"],"-",rgtab[i,"Name"])
      gname <- paste0(rgtab[i,"rgname"]," (RR=",sprintf("%5.2f",rgtab[i,"rr"]),")")      
      pop   <- rgtab[i,"scpop"]
      prev  <- rgtab[i,"prev"]
      res <- acfset(pop=pop,prev=prev,alg=alg,test=test)
      algname <- gsub(pattern = "X",replacement = "", rownames(res))
      ret <- cbind(expand.grid(gname,pop,prev,algname)[,1:3],algname,res)  
      names(ret) <- c("Group","Population","Prevalence","Algorithm","TP","FP","FN","TN","Cost","Cost per case")
      rownames(ret) <- NULL
      restab <- rbind(restab,ret)
    }
    restab$NNS <- ceiling(restab$Population / restab$TP)
    restab$Prevalent.Case <- round(restab$Population * restab$Prevalence /1e5,0)
    restab$Missed.Case <- round((restab$Prevalent.Case - restab$TP),0)
    restab$CDR <- round(restab$TP / restab$Prevalent.Case,2)
    values$screenres <- restab[,c("Group","Population","Prevalence","Prevalent.Case","Algorithm","TP","FP","FN","TN","Missed.Case","Cost","Cost per case","NNS")]
  })
  
  output$baseprev_o <- renderText({paste("Baseline prevalence: ",sprintf("%5.0f",values$bprev),"per 100 000")})
  
  output$cpop <- renderText({paste(" Country population: ",prettyNum(values$cpop,big.mark=",",scientific=F))})
  
  output$ref_rr <- renderText({
    paste0(values$rgrp[1,"rgname"], " (",values$rgrp[1,"mtxt_rr"],"), ", 
           values$rgrp[2,"rgname"], " (",values$rgrp[2,"mtxt_rr"],"), ",
           values$rgrp[3,"rgname"], " (",values$rgrp[3,"mtxt_rr"],"), ",
           values$rgrp[4,"rgname"], " (",values$rgrp[4,"mtxt_rr"],"), ",
           values$rgrp[5,"rgname"], " (",values$rgrp[5,"mtxt_rr"],"), ",
           values$rgrp[6,"rgname"], " (",values$rgrp[6,"mtxt_rr"],")"
    )
  })

  output$ref_accept <- renderText({
    paste0(values$rgrp[1,"rgname"], " (",values$rgrp[1,"mtxt_accept"],"), ", 
           values$rgrp[2,"rgname"], " (",values$rgrp[2,"mtxt_accept"],"), ",
           values$rgrp[3,"rgname"], " (",values$rgrp[3,"mtxt_accept"],"), ",
           values$rgrp[4,"rgname"], " (",values$rgrp[4,"mtxt_accept"],"), ",
           values$rgrp[5,"rgname"], " (",values$rgrp[5,"mtxt_accept"],"), ",
           values$rgrp[6,"rgname"], " (",values$rgrp[6,"mtxt_accept"],")"
    )
  })
  
  #-------------------------Fukushi
  output$rgname1o <- renderText({values$rgrp[1,"rgname"]})
  output$rr1o <- renderText({formatC(values$rgrp[1,"rr"], digits=3, format="fg")})
  output$prev1o <- renderText({values$rgrp[1,"prev"]})
  output$rgprv1o <- renderText({formatC(values$rgrp[1,"rgprv"], digits = 3, format = "fg")})
  output$pop1oo <- renderText({prettyNum(values$rgrp[1,"pop"],big.mark=",",scientific=F)})

  output$rgname2o <- renderText({values$rgrp[2,"rgname"]})
  output$rr2o <- renderText({formatC(values$rgrp[2,"rr"], digits=3, format="fg")})
  output$prev2o <- renderText({values$rgrp[2,"prev"]})
  output$rgprv2o <- renderText({formatC(values$rgrp[2,"rgprv"], digits = 3, format = "fg")})
  output$pop2oo <- renderText({prettyNum(values$rgrp[2,"pop"],big.mark=",",scientific=F)})

output$rgname3o <- renderText({values$rgrp[3,"rgname"]})
output$rr3o <- renderText({formatC(values$rgrp[3,"rr"], digits=3, format="fg")})
output$prev3o <- renderText({values$rgrp[3,"prev"]})
output$rgprv3o <- renderText({formatC(values$rgrp[3,"rgprv"], digits = 3, format = "fg")})
output$pop3oo <- renderText({prettyNum(values$rgrp[3,"pop"],big.mark=",",scientific=F)})

output$rgname4o <- renderText({values$rgrp[4,"rgname"]})
output$rr4o <- renderText({formatC(values$rgrp[4,"rr"], digits=3, format="fg")})
output$prev4o <- renderText({values$rgrp[4,"prev"]})
output$rgprv4o <- renderText({formatC(values$rgrp[4,"rgprv"], digits = 3, format = "fg")})
output$pop4oo <- renderText({prettyNum(values$rgrp[4,"pop"],big.mark=",",scientific=F)})

output$rgname5o <- renderText({values$rgrp[5,"rgname"]})
output$rr5o <- renderText({formatC(values$rgrp[5,"rr"], digits=3, format="fg")})
output$prev5o <- renderText({values$rgrp[5,"prev"]})
output$rgprv5o <- renderText({formatC(values$rgrp[5,"rgprv"], digits = 3, format = "fg")})
output$pop5oo <- renderText({prettyNum(values$rgrp[5,"pop"],big.mark=",",scientific=F)})

output$rgname6o <- renderText({values$rgrp[6,"rgname"]})
output$rr6o <- renderText({formatC(values$rgrp[6,"rr"], digits=3, format="fg")})
output$prev6o <- renderText({values$rgrp[6,"prev"]})
output$rgprv6o <- renderText({formatC(values$rgrp[6,"rgprv"], digits = 3, format = "fg")})
output$pop6oo <- renderText({prettyNum(values$rgrp[6,"pop"],big.mark=",",scientific=F)})

  #-------------------------

  output$gname1o <- renderText({values$rgrp[1,"rgname"]})
  output$gname2o <- renderText({values$rgrp[2,"rgname"]})
  output$gname3o <- renderText({values$rgrp[3,"rgname"]})
  output$gname4o <- renderText({values$rgrp[4,"rgname"]})
  output$gname5o <- renderText({values$rgrp[5,"rgname"]})
  output$gname6o <- renderText({values$rgrp[6,"rgname"]})
  
  output$pop1o <- renderText({prettyNum(values$rgrp[1,"pop"],big.mark=",",scientific=F)})
  output$pop2o <- renderText({prettyNum(values$rgrp[2,"pop"],big.mark=",",scientific=F)})
  output$pop3o <- renderText({prettyNum(values$rgrp[3,"pop"],big.mark=",",scientific=F)})
  output$pop4o <- renderText({prettyNum(values$rgrp[4,"pop"],big.mark=",",scientific=F)})
  output$pop5o <- renderText({prettyNum(values$rgrp[5,"pop"],big.mark=",",scientific=F)})
  output$pop6o <- renderText({prettyNum(values$rgrp[6,"pop"],big.mark=",",scientific=F)})
  
  output$scpop1 <- renderText({prettyNum(values$rgrp[1,"scpop"],big.mark=",",scientific=F)})
  output$scpop2 <- renderText({prettyNum(values$rgrp[2,"scpop"],big.mark=",",scientific=F)})
  output$scpop3 <- renderText({prettyNum(values$rgrp[3,"scpop"],big.mark=",",scientific=F)})
  output$scpop4 <- renderText({prettyNum(values$rgrp[4,"scpop"],big.mark=",",scientific=F)})
  output$scpop5 <- renderText({prettyNum(values$rgrp[5,"scpop"],big.mark=",",scientific=F)})
  output$scpop6 <- renderText({prettyNum(values$rgrp[6,"scpop"],big.mark=",",scientific=F)})
 
  output$tcode1 <- renderText({values$test[1,"n"]})
  output$tcode2 <- renderText({values$test[2,"n"]})
  output$tcode3 <- renderText({values$test[3,"n"]})
  output$tcode4 <- renderText({values$test[4,"n"]})
  output$tcode5 <- renderText({values$test[5,"n"]})
  output$tcode6 <- renderText({values$test[6,"n"]})
  output$tcode7 <- renderText({values$test[7,"n"]})
  output$tcode8 <- renderText({values$test[8,"n"]})
  output$tcode9 <- renderText({values$test[9,"n"]})
  output$tcode10 <- renderText({values$test[10,"n"]})
  output$tcode11 <- renderText({values$test[11,"n"]})
  output$tcode12 <- renderText({values$test[12,"n"]})
  output$tcode13 <- renderText({values$test[13,"n"]}) # FM
  output$tcode14 <- renderText({values$test[14,"n"]}) # FM
  output$tcode15 <- renderText({values$test[15,"n"]}) # FM

output$tname1 <- renderText({values$test[1,"desc"]})
output$tname2 <- renderText({values$test[2,"desc"]})
output$tname3 <- renderText({values$test[3,"desc"]})
output$tname4 <- renderText({values$test[4,"desc"]})
output$tname5 <- renderText({values$test[5,"desc"]})
output$tname6 <- renderText({values$test[6,"desc"]})
output$tname7 <- renderText({values$test[7,"desc"]})
output$tname8 <- renderText({values$test[8,"desc"]})
output$tname9 <- renderText({values$test[9,"desc"]})
output$tname10 <- renderText({values$test[10,"desc"]})
output$tname11 <- renderText({values$test[11,"desc"]})
output$tname12 <- renderText({values$test[12,"desc"]})
output$tname13 <- renderText({paste(" • Custom test 1:", values$test[13,"desc"], "   ", " ","( Sensitivity:", values$test[13, "sens"], ",   ", "Specificity:", values$test[13,"spec"], ",   ", "Diagnostic cost:", values$test[13,"dcost"], ",   ", "Operational cost:", values$test[13,"ocost"], ")", sep="  ")}) # FM
output$tname14 <- renderText({paste(" • Custom test 2:", values$test[14,"desc"], "   ", " ","( Sensitivity:", values$test[14, "sens"], ",   ", "Specificity:", values$test[14,"spec"], ",   ", "Diagnostic cost:", values$test[14,"dcost"], ",   ", "Operational cost:", values$test[14,"ocost"], ")", sep="  ")}) # FM
output$tname15 <- renderText({paste(" • Custom test 3:", values$test[15,"desc"], "   ", " ","( Sensitivity:", values$test[15, "sens"], ",   ", "Specificity:", values$test[15,"spec"], ",   ", "Diagnostic cost:", values$test[15,"dcost"], ",   ", "Operational cost:", values$test[15,"ocost"], ")", sep="  ")}) # FM



#======================================== 
#   output$algorithm <- renderPlot({
#     
#     numalg <- nrow(values$algdf)
#     openplotmat(main = "Diagnostic algorithm defined in the WHO guidelines")
#     pos <- diagram::coordinates(rep(5,numalg))
#     lab <- unlist(t(values$algdf[,1:5]))
#     
#     lab[lab=="sym_cgh2w"] <- "Cough >2wks"
#     lab[lab=="sym_any"] <- "Any TB symptoms"
#     lab[lab=="cxr_abn"] <- "X-ray (any abnormality)"
#     lab[lab=="ssm"] <- "Sputum smear microscopy"
#     lab[lab=="xpert"] <- "Xpert MTB/RIF"
#     
#     for(i in (0:(numalg-1)*5)){
#       textellipse(pos[i+1,], lab=lab[i+1], 
#                   box.col="lightblue",radx=0.08,rady=0.04)
#       if(lab[i+5]!="dummy"){
#         straightarrow (to = pos[i+5,], from= pos[i+4,],
#                        lwd = 2, arr.pos = 0.6, arr.length = 0.3)    
#         textrect(pos[i+5, ], lab=lab[i+5], radx=0.08,rady=0.04)
#       }
#       if(lab[i+4]=="dummy"){
#         straightarrow (to = pos[i+4,], from= pos[i+2,],
#                        lwd = 2, arr.pos = 0.5, arr.length = 0.3)
#         textrect(pos[i+4, ], lab=lab[i+3], radx=0.08,rady=0.04)
#       }else{
#         straightarrow (to = pos[i+3,], from= pos[i+2,],
#                        lwd = 2, arr.pos = 0.6, arr.length = 0.3)
#         straightarrow (to = pos[i+4,], from= pos[i+3,],
#                        lwd = 2, arr.pos = 0.6, arr.length = 0.3)
#         textrect(pos[i+3, ], lab=lab[i+3], radx=0.08,rady=0.04)
#         textrect(pos[i+4, ], lab=lab[i+4], radx=0.08,rady=0.04)    
#       }
#       textrect(pos[i+2, ], lab=lab[i+2], radx=0.08,rady=0.04)
#     }
#   }, width = 1200, height = 800)
#====================================  

#  output$summarytable <- renderTable({values$screenres})

  output$summarytable <- DT::renderDataTable(
    datatable(values$screenres,rownames = FALSE) %>% 
      formatCurrency(c('Cost','Cost per case'), interval=3, mark=",") %>%
      formatRound(2, 0)
    )

output$ui_cpc_select <- renderUI({
  grpall <- unique(as.character(values$screenres[,"Group"]))
  algchoice <- as.list(unique(as.character(values$screenres[,"Algorithm"])))
  names(algchoice) <- as.vector(sapply(algchoice, function(x){values$algdf[values$algdf[,"Alg"]==x,"desc"]}))
  list(
    checkboxGroupInput("grpsel1", "Group:", choices=grpall, selected = grpall),
    checkboxGroupInput("algsel1", "Algorithm:",choices=algchoice, selected = algchoice[1:10])
  )
})

output$ui_nns_select <- renderUI({
  grpall <- unique(as.character(values$screenres[,"Group"]))
  algchoice <- as.list(unique(as.character(values$screenres[,"Algorithm"])))
  names(algchoice) <- as.vector(sapply(algchoice, function(x){values$algdf[values$algdf[,"Alg"]==x,"desc"]}))
  list(
    checkboxGroupInput("grpsel2", "Group:", choices=grpall, selected = grpall),
    checkboxGroupInput("algsel2", "Algorithm:",choices=algchoice, selected = algchoice[1:10])
  )
})

output$ui_cvy_select <- renderUI({
  grpall <- unique(as.character(values$screenres[,"Group"]))
  algchoice <- as.list(unique(as.character(values$screenres[,"Algorithm"])))
  names(algchoice) <- as.vector(sapply(algchoice, function(x){values$algdf[values$algdf[,"Alg"]==x,"desc"]}))
  list(
    checkboxGroupInput("grpsel3", "Group:", choices=grpall, selected = grpall[c(2, 4)]),
    checkboxGroupInput("algsel3", "Algorithm:",choices=algchoice, selected = algchoice[1:10])
  )
})

output$ui_tpfp_select <- renderUI({
  grpall <- unique(as.character(values$screenres[,"Group"]))
  algchoice <- as.list(unique(as.character(values$screenres[,"Algorithm"])))
  names(algchoice) <- as.vector(sapply(algchoice, function(x){values$algdf[values$algdf[,"Alg"]==x,"desc"]}))
  list(
    checkboxGroupInput("grpsel4", "Group:", choices=grpall, selected = grpall[c(1:6)]),
    checkboxGroupInput("algsel4", "Algorithm:",choices=algchoice, selected = algchoice[1:10])
  )
})

output$ui_toty_select <- renderUI({
  grpall <- unique(as.character(values$screenres[,"Group"])) 
  algchoice <- as.list(unique(as.character(values$screenres[,"Algorithm"])))
  names(algchoice) <- as.vector(sapply(algchoice, function(x){values$algdf[values$algdf[,"Alg"]==x,"desc"]}))
  list(
    checkboxGroupInput("grpsel5", "Group:", choices=grpall, selected = grpall[c(2:6)]), #General populatio not to be selected by default 
    checkboxGroupInput("algsel5", "Algorithm:",choices=algchoice, selected = algchoice[c(1,10)])
  )
})

output$plot_cpc <- renderPlot({
  par(mar=c(4,6,4,1))
  restab <- values$screenres
  ngrp <- length(input$grpsel1)
  temp0 <- restab %>% filter(Algorithm %in% input$algsel1) %>% filter(Group %in% input$grpsel1)
  temp0$Algorithm <- factor(as.character(temp0$Algorithm))  
  max.cpc <- max(temp0[,"Cost per case"],na.rm=T)*1.1
  max.cpc <- ifelse(is.finite(max.cpc),max.cpc,200000)
  i <- 1
  temp <- temp0 %>% filter(Group==input$grpsel1[i])
  plot(1:length(temp[,"Algorithm"]),temp[,"Cost per case"],ann=F,axes=F,type="b",lwd=2,col=1,
       ylim=c(0,max.cpc))
  abline(h=0:100*100,col="lightblue",lty=3,lwd=1)
  abline(h=0:20*500,col="lightblue",lty=2,lwd=1)
  for(i in 2:ngrp){
    temp <- temp0 %>% filter(Group==input$grpsel1[i])
#    lines(as.numeric(temp[,"Algorithm"]),temp[,"Cost per case"],type="b",lwd=2,col=i,pch=1)  
    lines(1:length(temp[,"Algorithm"]),temp[,"Cost per case"],type="b",lwd=2,col=i,pch=1)
  }
#-------------------------- Fukushi revision
labels <- as.character(temp$Algorithm)
longest <- max(nchar(labels, type="chars", allowNA=F, keepNA =NA))
labels <- paste0(stri_sub(labels, 1,8),"\n", substrRight(labels, nchar(labels)-8), sep="")
labels <- sub("\n$", "", labels)
if (longest > 10) {las=1; cex.axis=0.8} else if (longest > 6) {las=1; cex.axis=0.9}else {las=1; cex.axis=1.2}

axis(1,at = 1:length(temp$Algorithm),labels=labels,tick=F, line=-1.4, cex.axis=cex.axis, las=las)
#---------------------------
  #axis(1,at = 1:length(temp$Algorithm),labels=temp$Algorithm,tick=F, line=-1.5) Fukushi
  axis(2,las=1)
  title(xlab="Algorithm", line=1)
  title(ylab="Cost per case detected (USD/case)",line=5)
  legend("topleft",bty="n",cex=1,col=c(1:ngrp),lty=1,lwd=2,pch=1,
         legend=input$grpsel1)
  title(main="Cost per case detected",line=1.2,cex.main=1.2)
  title(main=paste0("(baseline prevalence: ",temp0[1,"Prevalence"]," per 100 000)"),cex.main=1,line=0.2)
},width = 800, height = 800)  
  
output$down_cpc_nns <- downloadHandler(
  filename = function() { paste("ScreenTB_CPCNNS1_",Sys.Date(),".pdf",sep="") },
  content = function(file) {
    pdf(file,width=8,height=11)
    par(mfrow=c(2,1),cex=0.8,oma=c(1,1,1,1),mar=c(4,6,4,1))
###========================================================
#    par(mar=c(4,6,4,1))
    ngrp <- length(input$grpsel1)
    temp0 <- values$screenres %>% filter(Algorithm %in% input$algsel1) %>% filter(Group %in% input$grpsel1)
    temp0$Algorithm <- factor(as.character(temp0$Algorithm))  
###-------------------------------------------------------    
    max.cpc <- max(temp0[,"Cost per case"],na.rm=T)*1.1
    max.cpc <- ifelse(is.finite(max.cpc),max.cpc,200000)
    i <- 1
    temp <- temp0 %>% filter(Group==input$grpsel1[i])
    plot(1:length(temp[,"Algorithm"]),temp[,"Cost per case"],ann=F,axes=F,type="b",lwd=2,col=1,
         ylim=c(0,max.cpc))
#    abline(h=0:100*100,col="lightblue",lty=3,lwd=1)
    abline(h=0:20*500,col="lightblue",lty=2,lwd=1)
    for(i in 2:ngrp){
      temp <- temp0 %>% filter(Group==input$grpsel1[i])
      #    lines(as.numeric(temp[,"Algorithm"]),temp[,"Cost per case"],type="b",lwd=2,col=i,pch=1)  
      lines(1:length(temp[,"Algorithm"]),temp[,"Cost per case"],type="b",lwd=2,col=i,pch=1)
    }
    #-------------------------- Fukushi revision
    labels <- as.character(temp$Algorithm)
    longest <- max(nchar(labels, type="chars", allowNA=F, keepNA =NA))
    labels <- paste0(stri_sub(labels, 1,8),"\n", substrRight(labels, nchar(labels)-8), sep="")
    labels <- sub("\n$", "", labels)
    if (longest > 10) {las=1; cex.axis=0.8} else if (longest > 6) {las=1; cex.axis=0.9}else {las=1; cex.axis=1.2}
    
    axis(1,at = 1:length(temp$Algorithm),labels=labels,tick=F, line=-1.2, cex.axis=cex.axis, las=las)
    #---------------------------
    #axis(1,at = 1:length(temp$Algorithm),labels=temp$Algorithm,tick=F, line=-1.5)
    axis(2,las=1)
    title(xlab="Algorithm", line=1)
    title(ylab="Cost per case detected (USD/case)",line=5)
    legend("topleft",bty="n",cex=1,col=c(1:ngrp),lty=1,lwd=2,pch=1,
           legend=input$grpsel1)
    title(main="Cost per case detected",line=1.2,cex.main=1.2)
    title(main=paste0("(baseline prevalence: ",temp0[1,"Prevalence"]," per 100 000)"),cex.main=1,line=0.2)
###========================================================
   par(mar=c(4,6,4,1))
    max.nns <- max(temp0[,"NNS"],na.rm=T)*1.1
    max.nns <- ifelse(is.finite(max.nns),max.nns,50000)
    i <- 1
    temp <- temp0 %>% filter(Group==input$grpsel1[i])
    plot(1:length(temp[,"Algorithm"]),temp[,"NNS"],ann=F,axes=F,type="b",lwd=2,col=1,
         ylim=c(0,max.nns))
#    abline(h=0:25*100,col="lightblue",lty=3,lwd=1)
    abline(h=0:5*500,col="lightblue",lty=2,lwd=1)
    for(i in 2:ngrp){
      temp <- temp0 %>% filter(Group==input$grpsel1[i])
      lines(1:length(temp[,"Algorithm"]),temp[,"NNS"],type="b",lwd=2,col=i,pch=1)  
    }
    #-------------------------- Fukushi revision
    labels <- as.character(temp$Algorithm)
    longest <- max(nchar(labels, type="chars", allowNA=F, keepNA =NA))
    labels <- paste0(stri_sub(labels, 1,8),"\n", substrRight(labels, nchar(labels)-8), sep="")
    labels <- sub("\n$", "", labels)
    if (longest > 10) {las=1; cex.axis=0.8} else if (longest > 6) {las=1; cex.axis=0.9}else {las=1; cex.axis=1.2}
    
    axis(1,at = 1:length(temp$Algorithm),labels=labels,tick=F, line=-1.2, cex.axis=cex.axis, las=las)
    #---------------------------
    #axis(1,at = 1:length(temp$Algorithm),labels=temp$Algorithm,tick=F, line=-1.5)
    axis(2,las=1)
    title(xlab="Algorithm", line=1)
    title(ylab="Number needed to screen (NNS)",line=5)
    legend("topright",bty="n",cex=1,col=c(1:ngrp),lty=1,lwd=2,pch=1,
           legend=input$grpsel1)
    title(main="Number needed to screen",line=1.2,cex.main=1.2)
    title(main=paste0("(baseline prevalence: ",temp0[1,"Prevalence"]," per 100 000)"),cex.main=1,line=0.2)
    
#    mtext(text = "Potential yield of true-positive and false-positive cases of TB, by risk group and algorithm",side = 3,outer = TRUE,font=2, line=1.5)
    dev.off()
  }
)

output$plot_nns <- renderPlot({
  par(mar=c(4,6,4,1))
  ngrp <- length(input$grpsel2)  
  temp0 <- values$screenres %>% filter(Algorithm %in% input$algsel2) %>% filter(Group %in% input$grpsel2)
  temp0$Algorithm <- factor(as.character(temp0$Algorithm))
  max.nns <- max(temp0[,"NNS"],na.rm=T)*1.1
  max.nns <- ifelse(is.finite(max.nns),max.nns,50000)
  i <- 1
  temp <- temp0 %>% filter(Group==input$grpsel2[i])
  plot(1:length(temp[,"Algorithm"]),temp[,"NNS"],ann=F,axes=F,type="b",lwd=2,col=1,
       ylim=c(0,max.nns))
  abline(h=0:25*100,col="lightblue",lty=3,lwd=1)
  abline(h=0:5*500,col="lightblue",lty=2,lwd=1)
  for(i in 2:ngrp){
    temp <- temp0 %>% filter(Group==input$grpsel2[i])
    lines(1:length(temp[,"Algorithm"]),temp[,"NNS"],type="b",lwd=2,col=i,pch=1)  
  }
  #-------------------------- Fukushi revision
  labels <- as.character(temp$Algorithm)
  longest <- max(nchar(labels, type="chars", allowNA=F, keepNA =NA))
  labels <- paste0(stri_sub(labels, 1,8),"\n", substrRight(labels, nchar(labels)-8), sep="")
  labels <- sub("\n$", "", labels)
  if (longest > 10) {las=1; cex.axis=0.8} else if (longest > 6) {las=1; cex.axis=0.9}else {las=1; cex.axis=1.2}
  
  axis(1,at = 1:length(temp$Algorithm),labels=labels,tick=F, line=-1.4, cex.axis=cex.axis, las=las)
  #---------------------------
#  axis(1,at = 1:length(temp$Algorithm),labels=temp$Algorithm,tick=F, line=-1.5) # Fukushi
  axis(2,las=1)
  title(xlab="Algorithm", line=1)
  title(ylab="Number needed to screen (NNS)",line=5)
  legend("topright",bty="n",cex=1,col=c(1:ngrp),lty=1,lwd=2,pch=1,
         legend=input$grpsel2)
  title(main="Number needed to screen",line=1.2,cex.main=1.2)
  title(main=paste0("(baseline prevalence: ",temp0[1,"Prevalence"]," per 100 000)"),cex.main=1,line=0.2)
},width = 800, height = 800)  

output$down_cpc_nns2 <- downloadHandler(
  filename = function() { paste("ScreenTB_CPCNNS2_",Sys.Date(),".pdf",sep="") },
  content = function(file) {
    pdf(file,width=8,height=11)
    par(mfrow=c(2,1),cex=0.8,oma=c(1,1,1,1),mar=c(4,6,4,1))
    ###========================================================
    #    par(mar=c(4,6,4,1))
    ngrp <- length(input$grpsel2)
    temp0 <- values$screenres %>% filter(Algorithm %in% input$algsel2) %>% filter(Group %in% input$grpsel2)
    temp0$Algorithm <- factor(as.character(temp0$Algorithm))  
    ###-------------------------------------------------------    
    max.cpc <- max(temp0[,"Cost per case"],na.rm=T)*1.1
    max.cpc <- ifelse(is.finite(max.cpc),max.cpc,200000)
    i <- 1
    temp <- temp0 %>% filter(Group==input$grpsel2[i])
    plot(1:length(temp[,"Algorithm"]),temp[,"Cost per case"],ann=F,axes=F,type="b",lwd=2,col=1,
         ylim=c(0,max.cpc))
    #    abline(h=0:100*100,col="lightblue",lty=3,lwd=1)
    abline(h=0:20*500,col="lightblue",lty=2,lwd=1)
    for(i in 2:ngrp){
      temp <- temp0 %>% filter(Group==input$grpsel2[i])
      #    lines(as.numeric(temp[,"Algorithm"]),temp[,"Cost per case"],type="b",lwd=2,col=i,pch=1)  
      lines(1:length(temp[,"Algorithm"]),temp[,"Cost per case"],type="b",lwd=2,col=i,pch=1)
    }
    #-------------------------- Fukushi revision
    labels <- as.character(temp$Algorithm)
    longest <- max(nchar(labels, type="chars", allowNA=F, keepNA =NA))
    labels <- paste0(stri_sub(labels, 1,8),"\n", substrRight(labels, nchar(labels)-8), sep="")
    labels <- sub("\n$", "", labels)
    if (longest > 10) {las=1; cex.axis=0.8} else if (longest > 6) {las=1; cex.axis=0.9}else {las=1; cex.axis=1.2}
    
    axis(1,at = 1:length(temp$Algorithm),labels=labels,tick=F, line=-1.2, cex.axis=cex.axis, las=las)
    #---------------------------
    #axis(1,at = 1:length(temp$Algorithm),labels=temp$Algorithm,tick=F, line=-1.5)
    axis(2,las=1)
    title(xlab="Algorithm", line=1)
    title(ylab="Cost per case detected (USD/case)",line=5)
    legend("topleft",bty="n",cex=1,col=c(1:ngrp),lty=1,lwd=2,pch=1,
           legend=input$grpsel2)
    title(main="Cost per case detected",line=1.2,cex.main=1.2)
    title(main=paste0("(baseline prevalence: ",temp0[1,"Prevalence"]," per 100 000)"),cex.main=1,line=0.2)
    ###========================================================
    par(mar=c(4,6,4,1))
    max.nns <- max(temp0[,"NNS"],na.rm=T)*1.1
    max.nns <- ifelse(is.finite(max.nns),max.nns,50000)
    i <- 1
    temp <- temp0 %>% filter(Group==input$grpsel2[i])
    plot(1:length(temp[,"Algorithm"]),temp[,"NNS"],ann=F,axes=F,type="b",lwd=2,col=1,
         ylim=c(0,max.nns))
    #    abline(h=0:25*100,col="lightblue",lty=3,lwd=1)
    abline(h=0:5*500,col="lightblue",lty=2,lwd=1)
    for(i in 2:ngrp){
      temp <- temp0 %>% filter(Group==input$grpsel2[i])
      lines(1:length(temp[,"Algorithm"]),temp[,"NNS"],type="b",lwd=2,col=i,pch=1)  
    }
    #-------------------------- Fukushi revision
    labels <- as.character(temp$Algorithm)
    longest <- max(nchar(labels, type="chars", allowNA=F, keepNA =NA))
    labels <- paste0(stri_sub(labels, 1,8),"\n", substrRight(labels, nchar(labels)-8), sep="")
    labels <- sub("\n$", "", labels)
    if (longest > 10) {las=1; cex.axis=0.8} else if (longest > 6) {las=1; cex.axis=0.9}else {las=1; cex.axis=1.2}
    
    axis(1,at = 1:length(temp$Algorithm),labels=labels,tick=F, line=-1.2, cex.axis=cex.axis, las=las)
    #---------------------------
    #axis(1,at = 1:length(temp$Algorithm),labels=temp$Algorithm,tick=F, line=-1.5)
    axis(2,las=1)
    title(xlab="Algorithm", line=1)
    title(ylab="Number needed to screen (NNS)",line=5)
    legend("topright",bty="n",cex=1,col=c(1:ngrp),lty=1,lwd=2,pch=1,
           legend=input$grpsel2)
    title(main="Number needed to screen",line=1.2,cex.main=1.2)
    title(main=paste0("(baseline prevalence: ",temp0[1,"Prevalence"]," per 100 000)"),cex.main=1,line=0.2)
    
    #    mtext(text = "Potential yield of true-positive and false-positive cases of TB, by risk group and algorithm",side = 3,outer = TRUE,font=2, line=1.5)
    dev.off()
  }
)


output$plot_cvy <- renderPlot({
  restab <- values$screenres  
  par(mfrow=c(2,1),mar=c(4, 5, 4, 3)+0.2,oma=c(0,0,0,0))
  for(grploop in input$grpsel3){
    temp <- restab %>% filter(Group==grploop, Algorithm %in% input$algsel3)
    plot(temp$Cost/1000,temp$TP, ann=F,axes=F,xlim=c(0,max(temp$Cost)/1000),ylim=c(0,max(temp$Prevalent.Case)*1.1))
    abline(h = max(temp$Prevalent.Case),lty=2,col="blue")
    text(0,max(temp$Prevalent.Case)*0.98,col="blue",adj=c(0,1),cex=0.8,
         label=paste("Total prevalent cases:",max(temp$Prevalent.Case)))
    axis(1)
    axis(2,las=1)
    title(xlab="Total cost (thousand USD)")  
    title(ylab="True cases diagnosed",line=4)
    title(main=paste0(temp[1,1],"\n(Prevalence: ",temp[1,"Prevalence"],", ","Screened:",temp[1,"Population"],")"))  
    
    poly <- temp %>% arrange(Cost) %>% select(Cost,TP,Algorithm)
    dom <- poly[,1:2] %>% as.matrix() %>% dominated()
    dom.df <- cbind(poly,dom)
    front <- dom.df[dom.df$dom==FALSE,]
    domnt <- dom.df[dom.df$dom==TRUE,]
    lines(front[,"Cost"]/1000,front[,"TP"],lwd=2,col="blue")
    points(front[,"Cost"]/1000,front[,"TP"],pch=19,col="blue")
    points(domnt[,"Cost"]/1000,domnt[,"TP"],pch=4,col="red")
    
    dom.df$pos <- ifelse(dom.df$dom,4,3)
    dom.df$cex <- ifelse(dom.df$dom,0.9,1.1)
    #------------------Fukushi
    #labels <- as.character(dom.df$Algorithm)
    #longest <- max(nchar(labels, type="chars", allowNA=F, keepNA =NA))
    #labels <- paste0(stri_sub(labels, 1,8),"\n", substrRight(labels, nchar(labels)-8), sep="")
    #labels <- sub("\n$", "", labels)
    #text(dom.df$Cost/1000,dom.df$TP, labels=labels,cex=dom.df$cex,pos=dom.df$pos)
    #-------------------------
    text(dom.df$Cost/1000,dom.df$TP, labels=dom.df$Algorithm,cex=dom.df$cex,pos=dom.df$pos)
    
    legend("bottomright",pch=c(19,4),col=c("blue","red"),bty="n",lwd=2,lty=c(1,0),
           legend=c("Cost-effectiveness frontier","Dominated options"))
    
  }
},width = 800, height = 800)

output$down_cvy <- downloadHandler(
  filename = function() { paste("ScreenTB_CvsY_",Sys.Date(),".pdf",sep="") },
  content = function(file) {
    pdf(file,width=8,height=11)
    
    restab <- values$screenres  
    par(mfrow=c(2,1),mar=c(4, 5, 4, 3)+0.2,oma=c(1,1,2,1), cex=0.8)
    for(grploop in input$grpsel3){
      temp <- restab %>% filter(Group==grploop, Algorithm %in% input$algsel3)
      plot(temp$Cost/1000,temp$TP, ann=F,axes=F,xlim=c(0,max(temp$Cost)/1000),ylim=c(0,max(temp$Prevalent.Case)*1.1))
      abline(h = max(temp$Prevalent.Case),lty=2,col="blue")
      text(0,max(temp$Prevalent.Case)*0.98,col="blue",adj=c(0,1),cex=0.8,
           label=paste("Total prevalent cases:",max(temp$Prevalent.Case)))
      axis(1)
      axis(2,las=1)
      title(xlab="Total cost (thousand USD)")  
      title(ylab="True cases diagnosed",line=4)
      title(main=paste0(temp[1,1],"\n(Prevalence: ",temp[1,"Prevalence"],", ","Screened:",temp[1,"Population"],")"))  
      
      poly <- temp %>% arrange(Cost) %>% select(Cost,TP,Algorithm)
      dom <- poly[,1:2] %>% as.matrix() %>% dominated()
      dom.df <- cbind(poly,dom)
      front <- dom.df[dom.df$dom==FALSE,]
      domnt <- dom.df[dom.df$dom==TRUE,]
      lines(front[,"Cost"]/1000,front[,"TP"],lwd=2,col="blue")
      points(front[,"Cost"]/1000,front[,"TP"],pch=19,col="blue")
      points(domnt[,"Cost"]/1000,domnt[,"TP"],pch=4,col="red")
      
      dom.df$pos <- ifelse(dom.df$dom,4,3)
      dom.df$cex <- ifelse(dom.df$dom,0.9,1.1)
      text(dom.df$Cost/1000,dom.df$TP, labels=dom.df$Algorithm,cex=dom.df$cex,pos=dom.df$pos)
      
      legend("bottomright",pch=c(19,4),col=c("blue","red"),bty="n",lwd=2,lty=c(1,0),
             legend=c("Cost-effectiveness frontier","Dominated options"))
      
    }
    mtext(text = "Incremental costs and yield of screening across algorithms",side = 3,outer = TRUE,font=2, line=0.5)
    dev.off()
  }
)


output$plot_tpfp <- renderPlot({
  temp <- values$screenres %>% filter(Group %in% input$grpsel4, Algorithm %in% input$algsel4)
  ngrp <- length(input$grpsel4)

  par(mfrow=c(3,2),mar=c(6.5,4,3,1)) # , oma=c(4,1,2,1)
  for(i in 1:ngrp){
    temp2 <- temp %>% filter(Group==input$grpsel4[i]) %>%
      mutate(AllPos=TP+FP) %>%
      select(Group,Algorithm,TP,FP,FN,AllPos,Prevalent.Case,Population,Prevalence)
    ymax <- max(max(temp2[,"AllPos"],na.rm=T),temp2[1,"Prevalent.Case"],na.rm=T)
    xpos <- barplot(t(temp2[,3:4]),beside=F,axes=F, col=c("lightblue","pink"),ylim=c(0,ymax*1.1))
    #  axis(1,las=2,at=as.vector(xpos),labels=rep(c("TP","FP"),length(input$algsel3)),cex.axis=0.8,line=-1,tick=F) # Fukushi
    
    # --------------------------------- Fukushi revision
    labels <- input$algsel4
    longest <- max(nchar(labels, type="chars", allowNA=F, keepNA =NA))
    labels <- paste0(stri_sub(labels, 1,10),"\n", substrRight(labels, nchar(labels)-10), sep="")
    labels <- sub("\n$", "", labels)
     if (longest > 5) {las=2; cex.axis=1} else if (longest > 4) {las=1; cex.axis=1}else {las=1; cex.axis=1.2}
    axis(1,at=xpos, labels=labels,line=0.5,tick=F, las=las, cex.axis=cex.axis)
    #-------------------------------------------------- until here
    axis(2,las=2)
    abline(h=temp2[1,"Prevalent.Case"],col="blue",lwd=2,lty=3)
    legend("left",fill=c("pink","lightblue"),legend=c("False Positive","True Positive"),bty="n")
    text(x=mean(xpos),y=temp2[1,"Prevalent.Case"],labels="Maximum case detection (total prevelent cases)",pos=3,col="blue",cex=1.2)
    title(main=paste0(input$grpsel4[i],
                      "\n(Prevalence: ",temp2[1,"Prevalence"],", ","Screened:",temp2[1,"Population"],")")) 
  }
},width = 800, height = 900)

output$down_tpfp <- downloadHandler(
  filename = function() { paste("ScreenTB_TPFP_",Sys.Date(),".pdf",sep="") },
  content = function(file) {
    pdf(file,width=8,height=11)
    
    temp <- values$screenres %>% filter(Group %in% input$grpsel4, Algorithm %in% input$algsel4)
    ngrp <- length(input$grpsel4)
    par(mfrow=c(3,2),mar=c(4,4,4,1),cex=.5,oma=c(2,2,3.5,2))

    for(i in 1:ngrp){
      temp2 <- temp %>% filter(Group==input$grpsel4[i]) %>%
        mutate(AllPos=TP+FP) %>%
        select(Group,Algorithm,TP,FP,FN,AllPos,Prevalent.Case,Population,Prevalence)
      ymax <- max(max(temp2[,"AllPos"],na.rm=T),temp2[1,"Prevalent.Case"],na.rm=T)
      xpos <- barplot(t(temp2[,3:4]),beside=F,axes=F, col=c("lightblue","pink"),ylim=c(0,ymax*1.1))
      #  axis(1,las=2,at=as.vector(xpos),labels=rep(c("TP","FP"),length(input$algsel3)),
      #       cex.axis=0.8,line=-1,tick=F)
      
      # --------------------------------- Fukushi revision
      labels <- input$algsel4
      longest <- max(nchar(labels, type="chars", allowNA=F, keepNA =NA))
      labels <- paste0(stri_sub(labels, 1,9),"\n", substrRight(labels, nchar(labels)-9), sep="")
      labels <- sub("\n$", "", labels)
      if (longest > 5) {las=2; cex.axis=1} else if (longest > 4) {las=1; cex.axis=1}else {las=1; cex.axis=1.2}
      axis(1,at=xpos, labels=labels,line=0.5,tick=F, las=las, cex.axis=cex.axis)
      #-------------------------------------------------- until here
      #axis(1,at=xpos,labels=input$algsel4,line=0.5,tick=F) # Fukushi
      axis(2,las=2)
      abline(h=temp2[1,"Prevalent.Case"],col="blue",lwd=2,lty=3)
      legend("left",fill=c("pink","lightblue"),legend=c("False Positive","True Positive"),bty="n")
      text(x=mean(xpos),y=temp2[1,"Prevalent.Case"],labels="Maximum case detection (total prevelent cases)",pos=3,col="blue",cex=1.2)
      title(main=paste0(input$grpsel4[i],
                        "\n(Prevalence: ",temp2[1,"Prevalence"],", ","Screened:",temp2[1,"Population"],")")) 
    }

    mtext(text = "Potential yield of true-positive and false-positive cases of TB, by risk group and algorithm",side = 3,outer = TRUE,font=2, line=1.5)
    dev.off()
  }
)


pHeight5 <- function(){
  length(input$algsel5) * 400
}

output$plot_toty <- renderPlot({
  restab <- values$screenres  
  grp <- unique(restab$Group)
  alglst <- unique(restab$Algorithm)
  
  temp <- restab %>% filter(Group %in% input$grpsel5, Algorithm %in% input$algsel5)
  ngrp <- length(input$algsel5)
  
  #  mf <- switch(ngrp,c(1,1),c(2,1),c(2,2),c(2,2),c(3,2),c(3,2),c(4,2),c(4,2))
  #  ifelse(is.na(mf),c(1,1),mf)
  par(mfrow=c(ngrp,1),mar=c(0,10,7,7))
  for(i in 1:ngrp){
    temp2 <- temp %>% filter(Algorithm==input$algsel5[i]) %>%
      mutate(AllPos=TP+FP) %>%
      select(Group,Algorithm,TP,FP,FN,AllPos,Prevalent.Case,Population,Prevalence)
    xmax <- max(temp2[,"TP"],na.rm=T)
    ypos <- barplot(rev(temp2[,"TP"]),beside=F,axes=F, col=c("lightblue"),xlim=c(0,xmax*1.1),horiz = T)
    #  axis(1,las=2,at=as.vector(xpos),labels=rep(c("TP","FP"),length(input$algsel3)),
    #       cex.axis=0.8,line=-1,tick=F)
    grlab <- gsub(" \\(", "\n\\(",rev(input$grpsel5))
    grpop <- rev(paste0(temp2[,"TP"]," cases\n among ",
                        prettyNum(temp2[,"Population"],big.mark=",",scientific=F,preserve.width="none")," pop."))
    axis(2,las=2,at=ypos,labels=grlab,line=0.5,tick=F)
    axis(3,las=1)
    axis(4,las=2,at=ypos,labels=grpop,line=-3,tick=F)
#    abline(h=temp2[1,"Prevalent.Case"],col="blue",lwd=2,lty=3)
#    legend("left",fill=c("pink","lightblue"),legend=c("False Positive","True Positive"),bty="n")
#    text(x=mean(xpos),y=temp2[1,"Prevalent.Case"],labels="Maximum case detection (total prevelent cases)",pos=3,col="blue",cex=1.2)
    title(main=paste0("Algorithm ",input$algsel5[i],
                      ":  Estimated total yield (true positives) by risk group")) 
  }
},width = 800, height = pHeight5)

output$down_toty <- downloadHandler(
  filename = function() { paste("ScreenTB_Yield_",Sys.Date(),".pdf",sep="") },
  content = function(file) {
    #    country <- cnames[cnames$iso3==input$iso_1,"country"]
    pdf(file,width=8,height=11)

    restab <- values$screenres  
    temp <- restab %>% filter(Group %in% input$grpsel5, Algorithm %in% input$algsel5)
    ngrp <- length(input$algsel5)
    par(mfrow=c(2,1),mar=c(1,10,7,8),cex=.8,oma=c(0,0,1.5,0))
    for(i in 1:ngrp){
      temp2 <- temp %>% filter(Algorithm==input$algsel5[i]) %>%
        mutate(AllPos=TP+FP) %>%
        select(Group,Algorithm,TP,FP,FN,AllPos,Prevalent.Case,Population,Prevalence)
      xmax <- max(temp2[,"TP"],na.rm=T)
      ypos <- barplot(rev(temp2[,"TP"]),beside=F,axes=F, col=c("lightblue"),xlim=c(0,xmax*1.1),horiz = T)
      grlab <- gsub(" \\(", "\n\\(",rev(input$grpsel5))
      grpop <- rev(paste0(temp2[,"TP"]," cases\n among ",
                          prettyNum(temp2[,"Population"],big.mark=",",scientific=F,preserve.width="none")," pop."))
      axis(2,las=2,at=ypos,labels=grlab,line=0.5,tick=F)
      axis(3,las=1)
      axis(4,las=2,at=ypos,labels=grpop,line=-3,tick=F)
      title(main=paste0("Algorithm ",input$algsel5[i],
                        ":  Estimated total yield (true positives) by risk group")) 
    }
    mtext(text = "Total Yield",side = 3,outer = TRUE,font=2)
    dev.off()
  }
)


output$downloadData <- downloadHandler(
  filename = function() { paste('TBscreen_',Sys.Date(),'.csv', sep='') },
  content = function(file) {
    write.csv(values$screenres, file,row.names = FALSE)
  })

output$download_scenariofile <- downloadHandler(
  filename = function() { paste('Scenario_',input$baseiso,"_",Sys.Date(),'.Rdata', sep='') },
  content = function(file) {
    scenariofile <- list(values$rgrp,values$test,values$algdf, values$alg, 
                         input$baseiso, input$baseyear, input$baseprevmethod, input$baseprev_i, input$metatxt)
    names(scenariofile) <- c("rgrp","test","algdf","alg","baseiso","baseyear","baseprevmethod","baseprev_i","metatxt")
    save(scenariofile,file=file)
  }
)
######### Download from scenario storage
output$download_scenario1 <- downloadHandler(
  filename <- function() {
    paste("1_Scenario_KHM_additional_tools", "Rdata", sep=".")
  },
  content <- function(file){
    file.copy("./scenario_storage/1_Scenario_KHM_additional_tools.Rdata", file)  
  },
  contentType ="application/Rdata"
  )

output$download_scenario2_1 <- downloadHandler(
  filename <- function() {
    paste("2.1_Scenario_KHM_male_age", "Rdata", sep=".")
  },
  content <- function(file){
    file.copy("./scenario_storage/2.1_Scenario_KHM_male_age.Rdata", file)  
  },
  contentType ="application/Rdata"
)

output$download_scenario2_2 <- downloadHandler(
  filename <- function() {
    paste("2.2_Scenario_KHM_female_age", "Rdata", sep=".")
  },
  content <- function(file){
    file.copy("./scenario_storage/2.2_Scenario_KHM_female_age.Rdata", file)  
  },
  contentType ="application/Rdata"
)

output$download_scenario2_3 <- downloadHandler(
  filename <- function() {
    paste("2.3_Scenario_KHM_age", "Rdata", sep=".")
  },
  content <- function(file){
    file.copy("./scenario_storage/2.3_Scenario_KHM_age.Rdata", file)  
  },
  contentType ="application/Rdata"
)


## Output for  current settings ##
output$out_rgrp  <- renderTable({values$rgrp},include.rownames=FALSE)
output$out_test  <- renderTable({values$test},include.rownames=FALSE)
output$out_algdf <- renderTable({values$algdf},include.rownames=FALSE)

dataInput <- reactive({
  sessionEnvir <- sys.frame()
  if (!is.null(input$upload_scenariofile)) load(input$upload_scenariofile$datapath, sessionEnvir)
})

obs_upload <- observe({
  if (is.null(dataInput())){return()}else{
    dataInput()
    
    rgrptemp <- scenariofile[["rgrp"]]
    values$rgrp <- rgrptemp
    
    # ---- Fukushi --- to allow uploading previous scenario file
    if(nrow(scenariofile[["test"]])<14) {  
      combined.test <- rbind(scenariofile[["test"]][1:12,],
                             test.pre[13:15,],
                             scenariofile[["test"]][13,])
    
      rownames(combined.test) <- seq(length=nrow(combined.test))
      testtemp <- combined.test
    } else {
      testtemp <- scenariofile[["test"]]
    }
    # --------------------------------------------------
    # testtemp <- scenariofile[["test"]]  # Fukushi
    values$test <- testtemp
    
    algdftemp <- scenariofile[["algdf"]]
    values$algdf <- algdftemp
    
    values$alg <- scenariofile[["alg"]]
    
    isolate(updateSelectizeInput(session,"baseiso",selected=scenariofile[["baseiso"]])) 
    isolate(updateNumericInput(session,"baseyear",value=scenariofile[["baseyear"]])) 
    isolate(updateRadioButtons(session,"baseprevmethod",selected=scenariofile[["baseprevmethod"]])) 
    isolate(updateNumericInput(session,"baseprev_i",value=scenariofile[["baseprev_i"]])) 
    isolate(updateAceEditor(session, "metatxt", value=scenariofile[["metatxt"]]))
    

    {
    isolate(updateNumericInput(session,"sen1",value=testtemp[1,"sens"])) 
    isolate(updateNumericInput(session,"spe1",value=testtemp[1,"spec"])) 
    isolate(updateNumericInput(session,"dcost1",value=testtemp[1,"dcost"])) 
    isolate(updateNumericInput(session,"ocost1",value=testtemp[1,"ocost"])) 
    isolate(updateNumericInput(session,"sen2",value=testtemp[2,"sens"])) 
    isolate(updateNumericInput(session,"spe2",value=testtemp[2,"spec"])) 
    isolate(updateNumericInput(session,"dcost2",value=testtemp[2,"dcost"])) 
    isolate(updateNumericInput(session,"ocost2",value=testtemp[2,"ocost"])) 
    isolate(updateNumericInput(session,"sen3",value=testtemp[3,"sens"])) 
    isolate(updateNumericInput(session,"spe3",value=testtemp[3,"spec"])) 
    isolate(updateNumericInput(session,"dcost3",value=testtemp[3,"dcost"])) 
    isolate(updateNumericInput(session,"ocost3",value=testtemp[3,"ocost"])) 
    isolate(updateNumericInput(session,"sen4",value=testtemp[4,"sens"])) 
    isolate(updateNumericInput(session,"spe4",value=testtemp[4,"spec"])) 
    isolate(updateNumericInput(session,"dcost4",value=testtemp[4,"dcost"])) 
    isolate(updateNumericInput(session,"ocost4",value=testtemp[4,"ocost"])) 
    isolate(updateNumericInput(session,"sen8",  value=testtemp[5,"sens"])) 
    isolate(updateNumericInput(session,"spe5",  value=testtemp[5,"spec"])) 
    isolate(updateNumericInput(session,"dcost5",value=testtemp[5,"dcost"])) 
    isolate(updateNumericInput(session,"ocost5",value=testtemp[5,"ocost"])) 
    isolate(updateNumericInput(session,"sen6",  value=testtemp[6,"sens"])) 
    isolate(updateNumericInput(session,"spe6",  value=testtemp[6,"spec"])) 
    isolate(updateNumericInput(session,"dcost6",value=testtemp[6,"dcost"])) 
    isolate(updateNumericInput(session,"ocost6",value=testtemp[6,"ocost"])) 
    isolate(updateNumericInput(session,"sen7",  value=testtemp[7,"sens"])) 
    isolate(updateNumericInput(session,"spe7",  value=testtemp[7,"spec"])) 
    isolate(updateNumericInput(session,"dcost7",value=testtemp[7,"dcost"])) 
    isolate(updateNumericInput(session,"ocost7",value=testtemp[7,"ocost"])) 
    isolate(updateNumericInput(session,"sen8",  value=testtemp[8,"sens"])) 
    isolate(updateNumericInput(session,"spe8",  value=testtemp[8,"spec"])) 
    isolate(updateNumericInput(session,"dcost8",value=testtemp[8,"dcost"])) 
    isolate(updateNumericInput(session,"ocost8",value=testtemp[8,"ocost"])) 
    isolate(updateNumericInput(session,"sen9",  value=testtemp[9,"sens"])) 
    isolate(updateNumericInput(session,"spe9",  value=testtemp[9,"spec"])) 
    isolate(updateNumericInput(session,"dcost9",value=testtemp[9,"dcost"])) 
    isolate(updateNumericInput(session,"ocost9",value=testtemp[9,"ocost"])) 
    isolate(updateNumericInput(session,"sen10",  value=testtemp[10,"sens"])) 
    isolate(updateNumericInput(session,"spe10",  value=testtemp[10,"spec"])) 
    isolate(updateNumericInput(session,"dcost10",value=testtemp[10,"dcost"])) 
    isolate(updateNumericInput(session,"ocost10",value=testtemp[10,"ocost"])) 
    isolate(updateNumericInput(session,"sen11",  value=testtemp[11,"sens"])) 
    isolate(updateNumericInput(session,"spe11",  value=testtemp[11,"spec"])) 
    isolate(updateNumericInput(session,"dcost11",value=testtemp[11,"dcost"])) 
    isolate(updateNumericInput(session,"ocost11",value=testtemp[11,"ocost"]))
    isolate(updateNumericInput(session,"sen12",  value=testtemp[12,"sens"])) 
    isolate(updateNumericInput(session,"spe12",  value=testtemp[12,"spec"])) 
    isolate(updateNumericInput(session,"dcost12",value=testtemp[12,"dcost"])) 
    isolate(updateNumericInput(session,"ocost12",value=testtemp[12,"ocost"]))
    isolate(updateNumericInput(session,"sen13",  value=testtemp[13,"sens"])) 
    isolate(updateNumericInput(session,"spe13",  value=testtemp[13,"spec"])) 
    isolate(updateNumericInput(session,"dcost13",value=testtemp[13,"dcost"])) 
    isolate(updateNumericInput(session,"ocost13",value=testtemp[13,"ocost"]))
    isolate(updateNumericInput(session,"sen14",  value=testtemp[14,"sens"])) 
    isolate(updateNumericInput(session,"spe14",  value=testtemp[14,"spec"])) 
    isolate(updateNumericInput(session,"dcost14",value=testtemp[14,"dcost"])) 
    isolate(updateNumericInput(session,"ocost14",value=testtemp[14,"ocost"]))
    isolate(updateNumericInput(session,"sen15",  value=testtemp[15,"sens"])) 
    isolate(updateNumericInput(session,"spe15",  value=testtemp[15,"spec"])) 
    isolate(updateNumericInput(session,"dcost15",value=testtemp[15,"dcost"])) 
    isolate(updateNumericInput(session,"ocost15",value=testtemp[15,"ocost"]))} ### update all input for test 
    
    # Fukushi
    {
      isolate(updateTextInput(session,"ctname13",value=testtemp[13,"desc"]))
      isolate(updateTextInput(session,"ctname14",value=testtemp[14,"desc"]))
      isolate(updateTextInput(session,"ctname15",value=testtemp[15,"desc"]))
      
    }
    
    {
      isolate(updateTextInput(session,"algname1u",value=algdftemp[1,"Alg"])) 
      isolate(updateSelectInput(session,"s1test1",selected=algdftemp[1,"Step1"])) 
      isolate(updateSelectInput(session,"s2test1",selected=algdftemp[1,"Step2"])) 
      isolate(updateSelectInput(session,"d1test1",selected=algdftemp[1,"Step3"])) 
  
      isolate(updateTextInput(session,"algname2u",value=algdftemp[2,"Alg"])) 
      isolate(updateSelectInput(session,"s1test2",selected=algdftemp[2,"Step1"])) 
      isolate(updateSelectInput(session,"s2test2",selected=algdftemp[2,"Step2"])) 
      isolate(updateSelectInput(session,"d1test2",selected=algdftemp[2,"Step3"])) 
      
      isolate(updateTextInput(session,"algname3u",value=algdftemp[3,"Alg"])) 
      isolate(updateSelectInput(session,"s1test3",selected=algdftemp[3,"Step1"])) 
      isolate(updateSelectInput(session,"s2test3",selected=algdftemp[3,"Step2"])) 
      isolate(updateSelectInput(session,"d1test3",selected=algdftemp[3,"Step3"])) 
      
      isolate(updateTextInput(session,"algname4u",value=algdftemp[4,"Alg"])) 
      isolate(updateSelectInput(session,"s1test4",selected=algdftemp[4,"Step1"])) 
      isolate(updateSelectInput(session,"s2test4",selected=algdftemp[4,"Step2"])) 
      isolate(updateSelectInput(session,"d1test4",selected=algdftemp[4,"Step3"])) 
      
      isolate(updateTextInput(session,"algname5u",value=algdftemp[5,"Alg"])) 
      isolate(updateSelectInput(session,"s1test5",selected=algdftemp[5,"Step1"])) 
      isolate(updateSelectInput(session,"s2test5",selected=algdftemp[5,"Step2"])) 
      isolate(updateSelectInput(session,"d1test5",selected=algdftemp[5,"Step3"])) 
      
      isolate(updateTextInput(session,"algname6u",value=algdftemp[6,"Alg"])) 
      isolate(updateSelectInput(session,"s1test6",selected=algdftemp[6,"Step1"])) 
      isolate(updateSelectInput(session,"s2test6",selected=algdftemp[6,"Step2"])) 
      isolate(updateSelectInput(session,"d1test6",selected=algdftemp[6,"Step3"])) 
      
      isolate(updateTextInput(session,"algname7u",value=algdftemp[7,"Alg"])) 
      isolate(updateSelectInput(session,"s1test7",selected=algdftemp[7,"Step1"])) 
      isolate(updateSelectInput(session,"s2test7",selected=algdftemp[7,"Step2"])) 
      isolate(updateSelectInput(session,"d1test7",selected=algdftemp[7,"Step3"])) 
      
      isolate(updateTextInput(session,"algname8u",value=algdftemp[8,"Alg"])) 
      isolate(updateSelectInput(session,"s1test8",selected=algdftemp[8,"Step1"])) 
      isolate(updateSelectInput(session,"s2test8",selected=algdftemp[8,"Step2"])) 
      isolate(updateSelectInput(session,"d1test8",selected=algdftemp[8,"Step3"])) 
      
      isolate(updateTextInput(session,"algname9u",value=algdftemp[9,"Alg"])) 
      isolate(updateSelectInput(session,"s1test9",selected=algdftemp[9,"Step1"])) 
      isolate(updateSelectInput(session,"s2test9",selected=algdftemp[9,"Step2"])) 
      isolate(updateSelectInput(session,"d1test9",selected=algdftemp[9,"Step3"])) 
      
      isolate(updateTextInput(session,"algname10u",value=algdftemp[10,"Alg"])) 
      isolate(updateSelectInput(session,"s1test10",selected=algdftemp[10,"Step1"])) 
      isolate(updateSelectInput(session,"s2test10",selected=algdftemp[10,"Step2"])) 
      isolate(updateSelectInput(session,"d1test10",selected=algdftemp[10,"Step3"])) 
      
      isolate(updateTextInput(session,"algname11u",value=algdftemp[11,"Alg"])) 
      isolate(updateSelectInput(session,"s1test11",selected=algdftemp[11,"Step1"])) 
      isolate(updateSelectInput(session,"s2test11",selected=algdftemp[11,"Step2"])) 
      isolate(updateSelectInput(session,"d1test11",selected=algdftemp[11,"Step3"])) 
      
      isolate(updateTextInput(session,"algname12u",value=algdftemp[12,"Alg"])) 
      isolate(updateSelectInput(session,"s1test12",selected=algdftemp[12,"Step1"])) 
      isolate(updateSelectInput(session,"s2test12",selected=algdftemp[12,"Step2"])) 
      isolate(updateSelectInput(session,"d1test12",selected=algdftemp[12,"Step3"])) 
      
      #isolate(updateTextInput(session,"algname13u",value=algdftemp[13,"Alg"])) 
      #isolate(updateSelectInput(session,"s1test13",selected=algdftemp[13,"Step1"])) 
      #isolate(updateSelectInput(session,"s2test13",selected=algdftemp[13,"Step2"])) 
      #isolate(updateSelectInput(session,"d1test13",selected=algdftemp[13,"Step3"])) 
  } ### update all input for algorithm
    
    {
      isolate(updateSelectizeInput(session,"rglookup1",selected="---")) 
      isolate(updateTextInput(session,"gname1",value=rgrptemp[1,"rgname"])) 
      isolate(updateNumericInput(session,"rr1",value=rgrptemp[1,"rr"])) 
      isolate(updateNumericInput(session,"prv1",value=rgrptemp[1,"prev"])) 
      isolate(updateCheckboxInput(session,"fxp1",value=rgrptemp[1,"fixprev1"])) # Fukushi
      isolate(updateNumericInput(session,"rgprv1",value=rgrptemp[1,"rgprv"])) 
      isolate(updateNumericInput(session,"pop1",value=rgrptemp[1,"pop"])) 
      isolate(updateCheckboxInput(session,"fxpop1",value=rgrptemp[1,"fixpop"])) 
      isolate(updateNumericInput(session,"reach1",value=rgrptemp[1,"reach"])) 
      isolate(updateNumericInput(session,"accept1",value=rgrptemp[1,"accept"]))       

      isolate(updateSelectizeInput(session,"rglookup2",selected="---")) 
      isolate(updateTextInput(session,"gname2",value=rgrptemp[2,"rgname"])) 
      isolate(updateNumericInput(session,"rr2",value=rgrptemp[2,"rr"])) 
      isolate(updateNumericInput(session,"prv2",value=rgrptemp[2,"prev"])) 
      isolate(updateCheckboxInput(session,"fxp2",value=rgrptemp[2,"fixprev"])) # Fukushi
      isolate(updateNumericInput(session,"rgprv2",value=rgrptemp[2,"rgprv"])) 
      isolate(updateNumericInput(session,"pop2",value=rgrptemp[2,"pop"])) 
      isolate(updateCheckboxInput(session,"fxpop2",value=rgrptemp[2,"fixpop"])) 
      isolate(updateNumericInput(session,"reach2",value=rgrptemp[2,"reach"])) 
      isolate(updateNumericInput(session,"accept2",value=rgrptemp[2,"accept"]))       
      
      isolate(updateSelectizeInput(session,"rglookup3",selected="---")) 
      isolate(updateTextInput(session,"gname3",value=rgrptemp[3,"rgname"])) 
      isolate(updateNumericInput(session,"rr3",value=rgrptemp[3,"rr"])) 
      isolate(updateNumericInput(session,"prv3",value=rgrptemp[3,"prev"])) 
      isolate(updateCheckboxInput(session,"fxp3",value=rgrptemp[3,"fixprev"])) # Fukushi
      isolate(updateNumericInput(session,"rgprv3",value=rgrptemp[3,"rgprv"])) 
      isolate(updateNumericInput(session,"pop3",value=rgrptemp[3,"pop"])) 
      isolate(updateCheckboxInput(session,"fxpop3",value=rgrptemp[3,"fixpop"])) 
      isolate(updateNumericInput(session,"reach3",value=rgrptemp[3,"reach"])) 
      isolate(updateNumericInput(session,"accept3",value=rgrptemp[3,"accept"]))       

      isolate(updateSelectizeInput(session,"rglookup4",selected="---")) 
      isolate(updateTextInput(session,"gname4",value=rgrptemp[4,"rgname"])) 
      isolate(updateNumericInput(session,"rr4",value=rgrptemp[4,"rr"])) 
      isolate(updateNumericInput(session,"prv4",value=rgrptemp[4,"prev"])) 
      isolate(updateCheckboxInput(session,"fxp4",value=rgrptemp[4,"fixprev"])) # Fukushi 
      isolate(updateNumericInput(session,"rgprv4",value=rgrptemp[4,"rgprv"])) 
      isolate(updateNumericInput(session,"pop4",value=rgrptemp[4,"pop"])) 
      isolate(updateCheckboxInput(session,"fxpop4",value=rgrptemp[4,"fixpop"])) 
      isolate(updateNumericInput(session,"reach4",value=rgrptemp[4,"reach"])) 
      isolate(updateNumericInput(session,"accept4",value=rgrptemp[4,"accept"]))       
      
      isolate(updateSelectizeInput(session,"rglookup5",selected="---")) 
      isolate(updateTextInput(session,"gname5",value=rgrptemp[5,"rgname"])) 
      isolate(updateNumericInput(session,"rr5",value=rgrptemp[5,"rr"])) 
      isolate(updateNumericInput(session,"prv5",value=rgrptemp[5,"prev"])) 
      isolate(updateCheckboxInput(session,"fxp5",value=rgrptemp[5,"fixprev"])) # Fukushi 
      isolate(updateNumericInput(session,"rgprv5",value=rgrptemp[5,"rgprv"])) 
      isolate(updateNumericInput(session,"pop5",value=rgrptemp[5,"pop"])) 
      isolate(updateCheckboxInput(session,"fxpop5",value=rgrptemp[5,"fixpop"])) 
      isolate(updateNumericInput(session,"reach5",value=rgrptemp[5,"reach"])) 
      isolate(updateNumericInput(session,"accept5",value=rgrptemp[5,"accept"]))       

      isolate(updateSelectizeInput(session,"rglookup6",selected="---")) 
      isolate(updateTextInput(session,"gname6",value=rgrptemp[6,"rgname"])) 
      isolate(updateNumericInput(session,"rr6",value=rgrptemp[6,"rr"])) 
      isolate(updateNumericInput(session,"prv6",value=rgrptemp[6,"prev"])) 
      isolate(updateCheckboxInput(session,"fxp6",value=rgrptemp[6,"fixprev"])) # Fukushi 
      isolate(updateNumericInput(session,"rgprv6",value=rgrptemp[6,"rgprv"])) 
      isolate(updateNumericInput(session,"pop6",value=rgrptemp[6,"pop"])) 
      isolate(updateCheckboxInput(session,"fxpop6",value=rgrptemp[6,"fixpop"])) 
      isolate(updateNumericInput(session,"reach6",value=rgrptemp[6,"reach"])) 
      isolate(updateNumericInput(session,"accept6",value=rgrptemp[6,"accept"]))       
     
    } ### update all input for risk group and acceptability panels
  }
})

# Values from cdata returned as text
  output$clientdataText <- renderText({
    cnames <- names(cdata)
    cnames <- cnames[grepl("^url",cnames)]
    
    allvalues <- lapply(cnames, function(name) {
      paste(name, cdata[[name]], sep=" = ")
    })
    paste(allvalues, collapse = "\n")
  })

})







# output$allsummary <- renderPlot({
#   restab <- values$screenres  
#   par(mar=c(3,4,3,1)+0.5)
#   layout(matrix(c(1,1,2,2,1,1,2,2,3,3,5,6,4,4,7,8),4,4,byrow=TRUE))
#   ####################################
#   ngrp <- length(input$grpsel1)
#   temp0 <- restab %>% filter(Algorithm %in% input$algsel1)
#   temp0$Algorithm <- factor(as.character(temp0$Algorithm))
#   i <- 1
#   temp <- temp0 %>% filter(Group==input$grpsel1[i])
#   max.cpc <- max(temp[,"Cost per case"])*1.1    
#   plot(as.numeric(temp[,"Algorithm"]),temp[,"Cost per case"],ann=F,axes=F,type="b",lwd=2,col=1,
#        ylim=c(0,max.cpc))
#   abline(h=0:25*100,col="lightblue",lty=3,lwd=1)
#   abline(h=0:5*500,col="lightblue",lty=2,lwd=1)
#   for(i in 2:ngrp){
#     temp <- temp0 %>% filter(Group==input$grpsel1[i])
#     lines(as.numeric(temp[,"Algorithm"]),temp[,"Cost per case"],type="b",lwd=2,col=i,pch=1)  
#   }
#   axis(1,at = 1:length(temp$Algorithm),labels=temp$Algorithm,tick=F, line=-1.5)
#   axis(2,las=1)
#   title(xlab="Algorithm", line=1)
#   title(ylab="Cost per case detected (USD/case)")
#   legend("topleft",bty="n",cex=1,col=c(1:ngrp),lty=1,lwd=2,pch=1,
#          legend=input$grpsel1)
#   title(main="Cost per case detected",line=1.2,cex.main=1.2)
#   title(main=paste0("(baseline prevalence: ",temp0[1,"Prevalence"]," per 100 000)"),cex.main=1,line=0.2)
#   ####################################
#   ngrp <- length(input$grpsel1)
#   temp0 <- restab %>% filter(Algorithm %in% input$algsel1)
#   temp0$Algorithm <- factor(as.character(temp0$Algorithm))
#   i <- 1
#   temp <- temp0 %>% filter(Group==input$grpsel1[i])
#   max.cpc <- max(temp[,"Cost per case"])*1.1    
#   plot(as.numeric(temp[,"Algorithm"]),temp[,"Cost per case"],ann=F,axes=F,type="b",lwd=2,col=1,
#        ylim=c(0,max.cpc))
#   abline(h=0:25*100,col="lightblue",lty=3,lwd=1)
#   abline(h=0:5*500,col="lightblue",lty=2,lwd=1)
#   for(i in 2:ngrp){
#     temp <- temp0 %>% filter(Group==input$grpsel1[i])
#     lines(as.numeric(temp[,"Algorithm"]),temp[,"Cost per case"],type="b",lwd=2,col=i,pch=1)  
#   }
#   axis(1,at = 1:length(temp$Algorithm),labels=temp$Algorithm,tick=F, line=-1.5)
#   axis(2,las=1)
#   title(xlab="Algorithm", line=1)
#   title(ylab="Cost per case detected (USD/case)")
#   legend("topleft",bty="n",cex=1,col=c(1:ngrp),lty=1,lwd=2,pch=1,
#          legend=input$grpsel1)
#   title(main="Cost per case detected",line=1.2,cex.main=1.2)
#   title(main=paste0("(baseline prevalence: ",temp0[1,"Prevalence"]," per 100 000)"),cex.main=1,line=0.2)
#   ####################################
#   for(grploop in input$grpsel3[1,2]){
#     temp <- restab %>% filter(Group==grploop, Algorithm %in% input$algsel3)
#     plot(temp$Cost/1000,temp$TP, ann=F,axes=F,xlim=c(0,max(temp$Cost)/1000),ylim=c(0,max(temp$Prevalent.Case)*1.1))
#     abline(h = max(temp$Prevalent.Case),lty=2,col="blue")
#     text(0,max(temp$Prevalent.Case)*0.98,col="blue",adj=c(0,1),cex=0.8,
#          label=paste("Total prevalent cases:",max(temp$Prevalent.Case)))
#     axis(1)
#     axis(2,las=1)
#     title(xlab="Total cost (thousand USD)", ylab="True cases diagnosed",
#           main=paste0(temp[1,1],"\n(Prevalence: ",temp[1,"Prevalence"],", ","Screened:",temp[1,"Population"],")"))  
#     poly <- temp %>% arrange(Cost) %>% select(Cost,TP,Algorithm)
#     dom <- poly[,1:2] %>% as.matrix() %>% dominated()
#     dom.df <- cbind(poly,dom)
#     front <- dom.df[dom.df$dom==FALSE,]
#     domnt <- dom.df[dom.df$dom==TRUE,]
#     lines(front[,"Cost"]/1000,front[,"TP"],lwd=2,col="blue")
#     points(front[,"Cost"]/1000,front[,"TP"],pch=19,col="blue")
#     points(domnt[,"Cost"]/1000,domnt[,"TP"],pch=4,col="red")
#     dom.df$pos <- ifelse(dom.df$dom,4,3)
#     dom.df$cex <- ifelse(dom.df$dom,0.9,1.1)
#     text(dom.df$Cost/1000,dom.df$TP, labels=dom.df$Algorithm,cex=dom.df$cex,pos=dom.df$pos)
#     legend("bottomright",pch=c(19,4),col=c("blue","red"),bty="n",lwd=2,lty=c(1,0),
#            legend=c("Cost-effectiveness forntier","Dominated options"))
#   }
#   ####################################
#   grp <- unique(restab$Group)
#   alglst <- unique(restab$Algorithm)
#   temp <- restab %>% filter(Group %in% input$grpsel4, Algorithm %in% input$algsel4)
#   ngrp <- length(input$grpsel4)
#   for(i in 1:4){
#     temp2 <- temp %>% filter(Group==input$grpsel4[i]) %>%
#       mutate(AllPos=TP+FP) %>%
#       select(Group,Algorithm,TP,FP,FN,AllPos,Prevalent.Case,Population,Prevalence)
#     ymax <- max(max(temp2[,"AllPos"],na.rm=T),temp2[1,"Prevalent.Case"])
#     xpos <- barplot(t(temp2[,3:4]),beside=F,axes=F, col=c("lightblue","pink"),ylim=c(0,ymax*1.1))
#     #  axis(1,las=2,at=as.vector(xpos),labels=rep(c("TP","FP"),length(input$algsel3)),
#     #       cex.axis=0.8,line=-1,tick=F)
#     axis(1,at=xpos,labels=input$algsel4,line=0.5,tick=F)
#     axis(2,las=2)
#     abline(h=temp2[1,"Prevalent.Case"],col="blue",lwd=2,lty=3)
#     legend("left",fill=c("pink","lightblue"),legend=c("False Positive","True Positive"),bty="n")
#     text(x=mean(xpos),y=temp2[1,"Prevalent.Case"],labels="Maximum case detection (total prevelent cases)",pos=3,col="blue",cex=1.2)
#     title(main=paste0(input$grpsel4[i],
#                       "\n(Prevalence: ",temp2[1,"Prevalence"],", ","Screened:",temp2[1,"Population"],")")) 
#   }
#   ####################################
# },width = 800, height = 1200)





