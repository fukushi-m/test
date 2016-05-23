#library(markdown)

shinyUI(
  navbarPage(
    "ScreenTB - target prioritization and strategy selection for tuberculosis screening (active case finding)",
#    title=div(img(src="WHO-EN-C-H_shiny_48.jpeg"), "My Title in the Navbar"),
    tabPanel(
      "Risk Group",icon = icon("users"),
      fluidRow(
        tags$div(
          style="border-style: solid ; border-width: 1px; padding: 10px 10px 10px 10px;
                border-color:lightblue; 
          width:90%; margin-left:auto;margin-right:auto; border-radius: 10px;",
          #background-color: azure;
          p(tags$img(src="WHO-EN-C-H_onlylogo_75.jpg",align="left",hspace="30"),
            "This tool is a part of the WHO guidance on planning and implementation of systematic screening for active TB.",
            br(),
             "It should be used in conjunction with the document",
             a("Systematic screening for active tuberculosis: an operational guide.",href="http://who.int/tb/tbscreening/en/",target="_blank"),
            br(),
             "The operational guide includes an overview of the online tool, describes its limitations, and provides advice on how to use the tool and interpret its outputs.")
        ),
        tags$div(
          style="padding: 10px 20px 10px 20px;",
          h4("Define risk groups"),
          p("Specify the setting in which TB screening will be conducted by first selecting the country and year. This will populate the general population size and the TB prevalence with data from the WHO Global TB Report. If desired, the general TB prevalence can also be entered directly. Define the risk groups to be explored by selecting from the dropdown menus below. Set the population size and relative risk of TB for each risk group. Risk group population size can either be entered as an absolute number or as an estimated prevalence of risk factor in the general population. The TB prevalence in each risk group is calculated based on the TB prevalence in the general population for the specified country and year, and the relative risk for TB specified for each risk group."),
          br()
        )
      ),
      fluidRow(
        column(3,offset = 0,
               tags$div(
                 title="Specify the country in which screening will be conducted.",
                 h4("Country", align = "left",
                    tags$i(class="fa fa-info-circle", style="color:skyblue")
                 )
               )
        ),
        column(1,
               tags$div(
                 title="Specify the year of 'WHO Global TB Report' data on TB prevalence in the general population to be used.",
                 h4("Year", align = "center",
                    tags$i(class="fa fa-info-circle", style="color:skyblue")
                 )
               )
        ),
        column(7,
               tags$div(
                 title="If desired, directly enter the TB prevalence of the general population by selecting 'user input prevalence'.",
                 h4("Baseline prevalence", align = "center",
                    tags$i(class="fa fa-info-circle", style="color:skyblue")
                 )
               )
        )#,
#        column(2,
#               tags$div(
#                 title="If desired, directly enter the TB prevalence of the general population in the box below.",
#                 h5("User input prevalence", align = "left",
#                    tags$i(class="fa fa-info-circle", style="color:skyblue")
#                 )
#               )
#        )
      ),

      fluidRow(
        column(3,
               selectizeInput("baseiso",NA,choices=ls.isos, selected="KHM")), #rgrp.pre[1,5]
        column(1,
               numericInput("baseyear",NA,min=1990,max=2014,value=2014),
               tags$style(type='text/css', "#baseyear { width: 90px;}")
        ),
        column(5,align = "right",
#               tags$style(type='text/css', "#baseprevmethod { width: 400px; }"),
               radioButtons("baseprevmethod",label=NULL, 
                            choices = list("Use global TB estimate"=TRUE,"User input prevalence"=FALSE),inline=TRUE)
        ),
        column(2,
                numericInput("baseprev_i",NA,min=1,max=2000,value=668),
                tags$style(type='text/css', "#baseprev_i { width: 80px; }")
        ),
        tags$div(
            style="background-color: #F2F2F2;
            width: 3500px;
            height: 54px;"
        )
      ),
      
      fluidRow(
        column(3, offset=1, textOutput("cpop"), align = "right"),
        column(7, textOutput("baseprev_o"),br(), align = "center"),
        tags$head(tags$style("#cpop{color: #2E9AFE; font-size: 14px;} 
                       #baseprev_o{color: #2E9AFE; font-size: 14px;}") #font-style: italic;
        ),
        tags$div(
          style="background-color: #F2F2F2;
            width: 3500px;
            height: 28px;"
        )
      ),
br(),
br(),
      fluidRow(
        column(3,offset = 2,
               tags$div(
                 title="Select the risk groups to be explored from the dropdown menus. Selecting a risk group will prepopulate selected data in the tool specific to the risk group. Risk groups can be renamed by changing the text in the box 'Group name'.",
                 h4("Risk group", align = "left",
                    tags$i(class="fa fa-info-circle", style="color:skyblue")
                 )
               )
        ),
        column(3,
               tags$div(
                 title="Define the risk of TB within each risk group by specifying the relative risk of TB for the group compared to the general population in the box 'Rel. risk'. This will populate the TB prevalence for the group below the box 'TB prev.' For some risk groups the relative risk of TB will be prepopulated from systematic review data, which the user can change by entering a different value directly in the relative risk box. The prevalence of TB within the risk group can also be entered directly in the prevalence box by clicking the box labeled 'fix prev.'",
                 h4("TB Risk", align = "center",
                    tags$i(class="fa fa-info-circle", style="color:skyblue")
                 )
               )
        ),
        column(2,offset=0,
               tags$div(
                 title="Specify the population size of each risk group by entering the estimated percentage of the overall population that belongs to the risk group in the box 'Prev. (%)' (equivalent to the prevalence of the risk factor in the population). The absolute risk group size can also be entered directly into the box 'Pop. Size' by clicking the box labeled 'fix pop.'",
                 h4("Size of risk group", align = "center",
                    tags$i(class="fa fa-info-circle", style="color:skyblue")
                 )
               )
        )
      ),
      fluidRow(
        column(3,h5("Select group to load preset data")),
        column(2,h5("Group name")),
        column(1,h5("Rel. risk")),
        column(1,h5("TB prev.")),
        column(1,h5("fix prev.")),
        column(1,h5("Prev.(%)")),
        column(2,h5("Pop. Size")),
        column(1,h5("fix pop."))
      ),
      fluidRow(
#        column(1,p("Select"),
#               checkboxInput("gchk1",value=TRUE,label = "")),
        shinyjs::useShinyjs(),
        column(3,selectizeInput("rglookup1",label=NULL,choices=rglist[,"rgname"], selected=rgrp.pre[1,1])),       
        column(2,textInput("gname1", value=rgrp.pre[1,"rgname"], label=NULL)),
        column(1,numericInput("rr1", value=rgrp.pre[1,"rr"], step=0.1, label=NULL)),
        column(1,numericInput("prv1", value=rgrp.pre[1,"prev"], label=NULL)),
        column(1,checkboxInput("fxp1",value=rgrp.pre[1,"fixprev"],label=NULL)),
        column(1,numericInput("rgprv1", value=rgrp.pre[1,"rgprv"], label=NULL)),
        column(2,numericInput("pop1", value=rgrp.pre[1,"pop"], label=NULL)),
        column(1,checkboxInput("fxpop1",value=rgrp.pre[1,"fixpop"],label=NULL)),
         tags$style(type='text/css', "#gname1 { width: 160px; }"),
         tags$style(type='text/css', "#gname2 { width: 160px; }"),
         tags$style(type='text/css', "#gname3 { width: 160px; }"),
         tags$style(type='text/css', "#gname4 { width: 160px; }"),
         tags$style(type='text/css', "#gname5 { width: 160px; }"),
         tags$style(type='text/css', "#gname6 { width: 160px; }"),
         tags$style(type='text/css', "#rr1 { width: 80px; }"),
         tags$style(type='text/css', "#rr2 { width: 80px; }"),
         tags$style(type='text/css', "#rr3 { width: 80px; }"),
         tags$style(type='text/css', "#rr4 { width: 80px; }"),
         tags$style(type='text/css', "#rr5 { width: 80px; }"),
         tags$style(type='text/css', "#rr6 { width: 80px; }"),
        tags$style(type='text/css', "#rgprv1 { width: 80px; }"),
        tags$style(type='text/css', "#rgprv2 { width: 80px; }"),
        tags$style(type='text/css', "#rgprv3 { width: 80px; }"),
        tags$style(type='text/css', "#rgprv4 { width: 80px; }"),
        tags$style(type='text/css', "#rgprv5 { width: 80px; }"),
        tags$style(type='text/css', "#rgprv6 { width: 80px; }"),
        tags$style(type='text/css', "#prv1 { width: 80px; }"),
        tags$style(type='text/css', "#prv2 { width: 80px; }"),
        tags$style(type='text/css', "#prv3 { width: 80px; }"),
        tags$style(type='text/css', "#prv4 { width: 80px; }"),
        tags$style(type='text/css', "#prv5 { width: 80px; }"),
        tags$style(type='text/css', "#prv6 { width: 80px; }"),
        tags$style(type='text/css', "#pop1 { width: 110px; }"),
        tags$style(type='text/css', "#pop2 { width: 110px; }"),
        tags$style(type='text/css', "#pop3 { width: 110px; }"),
        tags$style(type='text/css', "#pop4 { width: 110px; }"),
        tags$style(type='text/css', "#pop5 { width: 110px; }"),
        tags$style(type='text/css', "#pop6 { width: 110px; }"),
        tags$div(
          style="background-color: #F2F2F2;
          width: 3500px;
          height: 54px;"
        )
      ),
###
#------------------------- Fukushi
fluidRow(
  column(2, offset=3, textOutput("rgname1o"), align="center"),
  column(1,textOutput("rr1o"), align="center"),
  column(1,textOutput("prev1o"), align="center"),
  column(1,offset=1,textOutput("rgprv1o"), align="center"),
  column(1,textOutput("pop1oo"), align="right"),
  tags$div(
    style="background-color: #F2F2F2;
          width: 3500px;
          height: 28px;"
  ),
  tags$head(tags$style("#rgname1o{color: #2E9AFE; font-size: 14px;} 
                       #rr1o{color: #2E9AFE; font-size: 14px;}
                       #prev1o{color: #2E9AFE; font-size: 14px;}
                       #rgprv1o{color: #2E9AFE; font-size: 14px;}
                       #pop1oo{color: #2E9AFE; font-size: 14px;}"
                       ) #font-style: italic;
  )
),
br(),
#-------------------------
      fluidRow(
#        column(1,checkboxInput("gchk2",value=TRUE,label="")),
        shinyjs::useShinyjs(),
        column(3,selectizeInput("rglookup2",label=NA,choices=rglist[,"rgname"], selected=rgrp.pre[2,"rgname"])),       
        column(2,textInput("gname2", value=rgrp.pre[2,"rgname"], label=NA)),
        column(1,numericInput("rr2", value=rgrp.pre[2,"rr"], step=0.1, label=NA)),
        column(1,numericInput("prv2", value=rgrp.pre[2,"prev"], label=NA)),
        column(1,checkboxInput("fxp2",value=rgrp.pre[2,"fixprev"],label="")),
        column(1,offset=0,numericInput("rgprv2", value=rgrp.pre[2,"rgprv"], label=NA)),
        column(2,numericInput("pop2", value=rgrp.pre[2,"pop"], label=NA)),
        column(1,checkboxInput("fxpop2",value=rgrp.pre[2,"fixpop"],label="")),
        tags$div(
          style="background-color: #F2F2F2;
          width: 3500px;
          height: 54px;"
        )
      ),
#------------------------- Fukushi
fluidRow(
  column(2, offset=3, textOutput("rgname2o"), align="center"),
  column(1,textOutput("rr2o"), align="center"),
  column(1,textOutput("prev2o"), align="center"),
  column(1,offset=1,textOutput("rgprv2o"), align="center"),
  column(1,textOutput("pop2oo"), align="right"),
  tags$div(
    style="background-color: #F2F2F2;
    width: 3500px;
    height: 28px;"
  ),
  tags$head(tags$style("#rgname2o{color: #2E9AFE; font-size: 14px;} 
                       #rr2o{color: #2E9AFE; font-size: 14px;}
                       #prev2o{color: #2E9AFE; font-size: 14px;}
                       #rgprv2o{color: #2E9AFE; font-size: 14px;}
                       #pop2oo{color: #2E9AFE; font-size: 14px;}"
  ) #font-style: italic;
  )
),
br(),
#-------------------------
      fluidRow(
#        column(1,checkboxInput("gchk3",value=TRUE,label="")),
        shinyjs::useShinyjs(),
        column(3,selectizeInput("rglookup3",label=NA,choices=rglist[,"rgname"], selected=rgrp.pre[3,"rgname"])),       
        column(2,textInput("gname3", value=rgrp.pre[3,"rgname"], label=NA)),
        column(1,numericInput("rr3", value=rgrp.pre[3,"rr"], step=0.1, label=NA)),
        column(1,numericInput("prv3", value=rgrp.pre[3,"prev"], label=NA)),
        column(1,checkboxInput("fxp3",value=rgrp.pre[3,"fixprev"],label="")),
        column(1,offset=0,numericInput("rgprv3", value=rgrp.pre[3,"rgprv"], label=NA)),
        column(2,numericInput("pop3", value=rgrp.pre[3,"pop"], label=NA)),
        column(1,checkboxInput("fxpop3",value=rgrp.pre[3,"fixpop"],label="")),
        tags$div(
          style="background-color: #F2F2F2;
          width: 3500px;
          height: 54px;"
        )
      ),
#------------------------- Fukushi
fluidRow(
  column(2, offset=3, textOutput("rgname3o"), align="center"),
  column(1,textOutput("rr3o"), align="center"),
  column(1,textOutput("prev3o"), align="center"),
  column(1,offset=1,textOutput("rgprv3o"), align="center"),
  column(1,textOutput("pop3oo"), align="right"),
  tags$div(
    style="background-color: #F2F2F2;
          width: 3500px;
          height: 28px;"
  ),
  tags$head(tags$style("#rgname3o{color: #2E9AFE; font-size: 14px;} 
                       #rr3o{color: #2E9AFE; font-size: 14px;}
                       #prev3o{color: #2E9AFE; font-size: 14px;}
                       #rgprv3o{color: #2E9AFE; font-size: 14px;}
                       #pop3oo{color: #2E9AFE; font-size: 14px;}"
  ) #font-style: italic;
  )
),
br(),
#-------------------------
      fluidRow(
#        column(1,checkboxInput("gchk4",value=TRUE,label="")),
        shinyjs::useShinyjs(),
        column(3,selectizeInput("rglookup4",label=NA,choices=rglist[,"rgname"], selected=rgrp.pre[4,"rgname"])),       
        column(2,textInput("gname4", value=rgrp.pre[4,"rgname"], label=NA)),
        column(1,numericInput("rr4", value=rgrp.pre[4,"rr"], step=0.1, label=NA)),
        column(1,numericInput("prv4", value=rgrp.pre[4,"prev"], label=NA)),
        column(1,checkboxInput("fxp4",value=rgrp.pre[4,"fixprev"],label="")),
        column(1,offset=0,numericInput("rgprv4", value=rgrp.pre[4,"rgprv"], label=NA)),
        column(2,numericInput("pop4", value=rgrp.pre[4,"pop"], label=NA)),
        column(1,checkboxInput("fxpop4",value=rgrp.pre[4,"fixpop"],label="")),
        tags$div(
          style="background-color: #F2F2F2;
          width: 3500px;
          height: 54px;"
        )
      ),

#------------------------- Fukushi
fluidRow(
  column(2, offset=3, textOutput("rgname4o"), align="center"),
  column(1,textOutput("rr4o"), align="center"),
  column(1,textOutput("prev4o"), align="center"),
  column(1,offset=1,textOutput("rgprv4o"), align="center"),
  column(1,textOutput("pop4oo"), align="right"),
  tags$div(
    style="background-color: #F2F2F2;
          width: 3500px;
          height: 28px;"
  ),
  tags$head(tags$style("#rgname4o{color: #2E9AFE; font-size: 14px;} 
                       #rr4o{color: #2E9AFE; font-size: 14px;}
                       #prev4o{color: #2E9AFE; font-size: 14px;}
                       #rgprv4o{color: #2E9AFE; font-size: 14px;}
                       #pop4oo{color: #2E9AFE; font-size: 14px;}"
  ) #font-style: italic;
  )
),
br(),
#-------------------------
      fluidRow(
 #       column(1,checkboxInput("gchk5",value=TRUE,label="")),
        shinyjs::useShinyjs(),
        column(3,selectizeInput("rglookup5",label=NA,choices=rglist[,"rgname"], selected=rgrp.pre[5,"rgname"])),       
        column(2,textInput("gname5", value=rgrp.pre[5,"rgname"], label=NA)),
        column(1,numericInput("rr5", value=rgrp.pre[5,"rr"], step=0.1, label=NA)),
        column(1,numericInput("prv5", value=rgrp.pre[5,"prev"], label=NA)),
        column(1,checkboxInput("fxp5",value=rgrp.pre[5,"fixprev"],label="")),
        column(1,offset=0,numericInput("rgprv5", value=rgrp.pre[5,"rgprv"], label=NA)),
        column(2,numericInput("pop5", value=rgrp.pre[5,"pop"], label=NA)),
        column(1,checkboxInput("fxpop5",value=rgrp.pre[5,"fixpop"],label="")),
        tags$div(
          style="background-color: #F2F2F2;
          width: 3500px;
          height: 54px;"
        )
      ),
#------------------------- Fukushi
fluidRow(
  column(2, offset=3, textOutput("rgname5o"), align="center"),
  column(1,textOutput("rr5o"), align="center"),
  column(1,textOutput("prev5o"), align="center"),
  column(1,offset=1,textOutput("rgprv5o"), align="center"),
  column(1,textOutput("pop5oo"), align="right"),
  tags$div(
    style="background-color: #F2F2F2;
          width: 3500px;
          height: 28px;"
  ),
  tags$head(tags$style("#rgname5o{color: #2E9AFE; font-size: 14px;} 
                       #rr5o{color: #2E9AFE; font-size: 14px;}
                       #prev5o{color: #2E9AFE; font-size: 14px;}
                       #rgprv5o{color: #2E9AFE; font-size: 14px;}
                       #pop5oo{color: #2E9AFE; font-size: 14px;}"
  ) #font-style: italic;
  )
),
br(),
#-------------------------
      fluidRow(
#        column(1,checkboxInput("gchk6",value=TRUE,label="")),
        shinyjs::useShinyjs(),
        column(3,selectizeInput("rglookup6",label=NA,choices=rglist[,"rgname"], selected=rgrp.pre[6,"rgname"])),       
        column(2,textInput("gname6", value=rgrp.pre[6,"rgname"], label=NA)),
        column(1,numericInput("rr6", value=rgrp.pre[6,"rr"], step=0.1, label=NA)),
        column(1,numericInput("prv6", value=rgrp.pre[6,"prev"], label=NA)),
        column(1,checkboxInput("fxp6",value=rgrp.pre[6,"fixprev"],label="")),
        column(1,offset=0,numericInput("rgprv6", value=rgrp.pre[6,"rgprv"], label=NA)),
        column(2,numericInput("pop6", value=rgrp.pre[6,"pop"], label=NA)),
        column(1,checkboxInput("fxpop6",value=rgrp.pre[6,"fixpop"],label="")),
        tags$div(
          style="background-color: #F2F2F2;
          width: 3500px;
          height: 54px;"
        )
       ),
#------------------------- Fukushi
fluidRow(
  column(2, offset=3, textOutput("rgname6o"), align="center"),
  column(1,textOutput("rr6o"), align="center"),
  column(1,textOutput("prev6o"), align="center"),
  column(1,offset=1,textOutput("rgprv6o"), align="center"),
  column(1,textOutput("pop6oo"), align="right"),
  tags$div(
    style="background-color: #F2F2F2;
          width: 3500px;
          height: 28px;"
  ),
  tags$head(tags$style("#rgname6o{color: #2E9AFE; font-size: 14px;} 
                       #rr6o{color: #2E9AFE; font-size: 14px;}
                       #prev6o{color: #2E9AFE; font-size: 14px;}
                       #rgprv6o{color: #2E9AFE; font-size: 14px;}
                       #pop6oo{color: #2E9AFE; font-size: 14px;}"
  ) #font-style: italic;
  )
),
br(),
#-------------------------
      fluidRow(
        column(3,offset=0,actionButton("defgrp", "  Reload default groups", icon = icon("refresh"))),
        column(5,offset=0,
               tags$div(
                 h5("Reference for default risk values: "),
                 h6(htmlOutput("ref_rr"))
                 )
               )
      )
),

tabPanel(
  "Acceptability",icon = icon("ban"),
  fluidRow(
    tags$div(
      style="padding: 10px 20px 10px 20px;",
      h4("Reachability & Acceptability"),
      p("Calculation of the estimated number of people to be screened in each risk group takes into account the proportion of each risk group that could be reached by a screening programme, and then once reached, the proportion of each risk group likely to agree to screening."),
      br()
    )
  ),
  fluidRow(
    column(2,offset=1,
           tags$div(
             title="As defined in the risk group panel",
             strong("Group name",tags$i(class="fa fa-info-circle", style="color:skyblue"))
           )
    ),
    column(2,
           tags$div(
             title="As defined in the risk group panel",
             strong("Pop. Size",tags$i(class="fa fa-info-circle", style="color:skyblue")), align="right"
           )
    ),
    column(2,
           tags$div(
             title="Specify the estimated proportion of each risk group that could be reached through a screening programme.",
             strong("Reachability (%)",tags$i(class="fa fa-info-circle", style="color:skyblue"))
           )
    ),
    column(2,
           tags$div(
             title="Specify the estimated proportion of each risk group that, once reached, would agree to screening. For some risk groups, this is prepopulated with estimates of the proportion that find screening acceptable from systematic review data, which the user can change by entering a different value directly into the box 'Acceptability (%)'.",
             strong("Acceptability (%)",tags$i(class="fa fa-info-circle", style="color:skyblue"))
           )
    ),
    column(2,
           tags$div(
             title="Displays the calculation of the estimated number to be screened, based on the population size, 
             the reach of the screening program, and the acceptability to screening within the risk group.",
             strong("To be screened",tags$i(class="fa fa-info-circle", style="color:skyblue")), align="right"
           )
    )
  ),

  fluidRow(
    column(2, offset=1, textOutput("gname1o")),
    column(2,textOutput("pop1o"), align="right"),
    column(2, numericInput("reach1", value=rgrp.pre[1,"reach"], label=NA)),
    column(2,numericInput("accept1", value=rgrp.pre[1,"accept"], label=NA)),
    column(2,textOutput("scpop1"), align="right"),
      tags$style(type='text/css', "#reach1 { width: 90px; }"),
      tags$style(type='text/css', "#reach2 { width: 90px; }"),
      tags$style(type='text/css', "#reach3 { width: 90px; }"),
      tags$style(type='text/css', "#reach4 { width: 90px; }"),
      tags$style(type='text/css', "#reach5 { width: 90px; }"),
      tags$style(type='text/css', "#reach6 { width: 90px; }"),
      tags$style(type='text/css', "#accept1 { width: 90px; }"),
      tags$style(type='text/css', "#accept2 { width: 90px; }"),
      tags$style(type='text/css', "#accept3 { width: 90px; }"),
      tags$style(type='text/css', "#accept4 { width: 90px; }"),
      tags$style(type='text/css', "#accept5 { width: 90px; }"),
      tags$style(type='text/css', "#accept6 { width: 90px; }")
  ),
  fluidRow(
    column(2, offset=1,textOutput("gname2o")),
    column(2,textOutput("pop2o"), align="right"),
    column(2,offset=0,numericInput("reach2", value=rgrp.pre[2,"reach"], label=NA)),
    column(2,offset=0,numericInput("accept2", value=rgrp.pre[2,"accept"], label=NA)),
    column(2,textOutput("scpop2"), align="right")
  ),
  fluidRow(
    column(2,offset=1, textOutput("gname3o")),
    column(2,textOutput("pop3o"), align="right"),
    column(2,offset=0,numericInput("reach3", value=rgrp.pre[3,"reach"], label=NA)),
    column(2,offset=0,numericInput("accept3", value=rgrp.pre[3,"accept"], label=NA)),
    column(2,textOutput("scpop3"), align="right")
  ),
  fluidRow(
    column(2,offset=1,textOutput("gname4o")),
    column(2,textOutput("pop4o"), align="right"),
    column(2,offset=0,numericInput("reach4", value=rgrp.pre[4,"reach"], label=NA)),
    column(2,offset=0,numericInput("accept4", value=rgrp.pre[4,"accept"], label=NA)),
    column(2,textOutput("scpop4"), align="right")
  ),
  fluidRow(
    column(2,offset=1,textOutput("gname5o")),
    column(2,textOutput("pop5o"), align="right"),
    column(2,offset=0,numericInput("reach5", value=rgrp.pre[5,"reach"], label=NA)),
    column(2,offset=0,numericInput("accept5", value=rgrp.pre[5,"accept"], label=NA)),
    column(2,textOutput("scpop5"), align="right")
  ),
  fluidRow(
    column(2,offset=1, textOutput("gname6o")),
    column(2,textOutput("pop6o"), align="right"),
    column(2,offset=0,numericInput("reach6", value=rgrp.pre[6,"reach"], label=NA)),
    column(2,offset=0,numericInput("accept6", value=rgrp.pre[6,"accept"], label=NA)),
    column(2,textOutput("scpop6"), align="right")
  ),
  fluidRow(
    column(7,offset=5,
           tags$div(
             h5("Reference for default values of acceptability: "),
             h6(htmlOutput("ref_accept"))
           )
    )
  )
),

tabPanel(
  "Test accuracy and costs",icon = icon("search"),
  fluidRow(
    tags$div(
      style="padding: 10px 20px 10px 20px;",
      h4("Define test accuracy and cost"),
      p("For each screening and diagnostic test (method), the test accuracy is defined by the sensitivity (the proportion of those with TB correctly identified) and specificity (the proportion of those without TB correctly identified). Values for the sensitivity and specificity of each screening and diagnostic test are preset, based on systematic review data. Locally appropriate estimates for the cost of applying the screening or diagnostic test per each person tested (diagnostic and operational costs separately) should be entered for each test."),
      br()
    )
  ),
  fluidRow(
    column(3,strong("Test description")),
    column(2,
           tags$div(
             title="The proportion of people with TB correctly identified by the test.",
             strong("Sensitivity",tags$i(class="fa fa-info-circle", style="color:skyblue"))
           )
    ),
    column(2,
           tags$div(
             title="The proportion of people without TB correctly identified by the test.",
             strong("Specificity",tags$i(class="fa fa-info-circle", style="color:skyblue"))
           )
    ),
    column(2,
           tags$div(
             title="Locally appropriate estimate of the unit cost (diagnostic cost) for the test per person tested.",
             strong("Diagnostic cost",tags$i(class="fa fa-info-circle", style="color:skyblue"))
           )
    ),
    column(2,
           tags$div(
             title="Locally appropriate estimate of operational cost required to conduct screening, expressed as cost per person tested.",
             strong("Operational cost",tags$i(class="fa fa-info-circle", style="color:skyblue"))
           )
    ),
    column(1,
           tags$div(
             title="Short code for each tool (to be used in the graph output panels)",
             strong("Code",tags$i(class="fa fa-info-circle", style="color:skyblue"))
           )
    )
  ),

  fluidRow(
    tags$div(
      style="padding: 10px 0px 10px 15px;",
      h5(tags$b(tags$i("Screening test - Chest radiography")))
    )
  ),
  fluidRow(
    column(3,textOutput("tname1"), align="left"),
    column(2,numericInput("sen1", value=test.pre[1,"sens"], step=0.01, label=NA)),
    column(2,numericInput("spe1", value=test.pre[1,"spec"], step=0.01, label=NA)),
    column(2,numericInput("dcost1", value=test.pre[1,"dcost"], step=0.1, label=NA)),
    column(2,numericInput("ocost1", value=test.pre[1,"ocost"], step=0.1, label=NA)),
    column(1,textOutput("tcode1"), align="left"),
    tags$style(type='text/css', "#sen1 { width: 90px; }"),
    tags$style(type='text/css', "#sen2 { width: 90px; }"),
    tags$style(type='text/css', "#sen3 { width: 90px; }"),
    tags$style(type='text/css', "#sen4 { width: 90px; }"),
    tags$style(type='text/css', "#sen5 { width: 90px; }"),
    tags$style(type='text/css', "#sen6 { width: 90px; }"),
    tags$style(type='text/css', "#sen7 { width: 90px; }"),
    tags$style(type='text/css', "#sen8 { width: 90px; }"),
    tags$style(type='text/css', "#sen9 { width: 90px; }"),
    tags$style(type='text/css', "#sen10 { width: 90px; }"),
    tags$style(type='text/css', "#sen11 { width: 90px; }"),
    tags$style(type='text/css', "#sen12 { width: 90px; }"),
    tags$style(type='text/css', "#sen13 { width: 90px; }"),
    tags$style(type='text/css', "#sen14 { width: 90px; }"),
    tags$style(type='text/css', "#sen15 { width: 90px; }"),
    tags$style(type='text/css', "#spe1 { width: 90px; }"),
    tags$style(type='text/css', "#spe2 { width: 90px; }"),
    tags$style(type='text/css', "#spe3 { width: 90px; }"),
    tags$style(type='text/css', "#spe4 { width: 90px; }"),
    tags$style(type='text/css', "#spe5 { width: 90px; }"),
    tags$style(type='text/css', "#spe6 { width: 90px; }"),
    tags$style(type='text/css', "#spe7 { width: 90px; }"),
    tags$style(type='text/css', "#spe8 { width: 90px; }"),
    tags$style(type='text/css', "#spe9 { width: 90px; }"),
    tags$style(type='text/css', "#spe10 { width: 90px; }"),
    tags$style(type='text/css', "#spe11 { width: 90px; }"),
    tags$style(type='text/css', "#spe12 { width: 90px; }"),
    tags$style(type='text/css', "#spe13 { width: 90px; }"),
    tags$style(type='text/css', "#spe14 { width: 90px; }"),
    tags$style(type='text/css', "#spe15 { width: 90px; }"),
    tags$style(type='text/css', "#dcost1 { width: 90px; }"),
    tags$style(type='text/css', "#dcost2 { width: 90px; }"),
    tags$style(type='text/css', "#dcost3 { width: 90px; }"),
    tags$style(type='text/css', "#dcost4 { width: 90px; }"),
    tags$style(type='text/css', "#dcost5 { width: 90px; }"),
    tags$style(type='text/css', "#dcost6 { width: 90px; }"),
    tags$style(type='text/css', "#dcost7 { width: 90px; }"),
    tags$style(type='text/css', "#dcost8 { width: 90px; }"),
    tags$style(type='text/css', "#dcost9 { width: 90px; }"),
    tags$style(type='text/css', "#dcost10 { width: 90px; }"),
    tags$style(type='text/css', "#dcost11 { width: 90px; }"),
    tags$style(type='text/css', "#dcost12 { width: 90px; }"),
    tags$style(type='text/css', "#dcost13 { width: 90px; }"),
    tags$style(type='text/css', "#dcost14 { width: 90px; }"),
    tags$style(type='text/css', "#dcost15 { width: 90px; }"),
    tags$style(type='text/css', "#ocost1 { width: 90px; }"),
    tags$style(type='text/css', "#ocost2 { width: 90px; }"),
    tags$style(type='text/css', "#ocost3 { width: 90px; }"),
    tags$style(type='text/css', "#ocost4 { width: 90px; }"),
    tags$style(type='text/css', "#ocost5 { width: 90px; }"),
    tags$style(type='text/css', "#ocost6 { width: 90px; }"),
    tags$style(type='text/css', "#ocost7 { width: 90px; }"),
    tags$style(type='text/css', "#ocost8 { width: 90px; }"),
    tags$style(type='text/css', "#ocost9 { width: 90px; }"),
    tags$style(type='text/css', "#ocost10 { width: 90px; }"),
    tags$style(type='text/css', "#ocost11 { width: 90px; }"),
    tags$style(type='text/css', "#ocost12 { width: 90px; }"),
    tags$style(type='text/css', "#ocost13 { width: 90px; }"),
    tags$style(type='text/css', "#ocost14 { width: 90px; }"),
    tags$style(type='text/css', "#ocost15 { width: 90px; }")
  ),
  fluidRow(
    column(3,textOutput("tname2"), align="left"),
    column(2,numericInput("sen2", value=test.pre[2,"sens"], step=0.01, label=NA)),
    column(2,numericInput("spe2", value=test.pre[2,"spec"], step=0.01, label=NA)),
    column(2,numericInput("dcost2", value=test.pre[2,"dcost"], step=0.1, label=NA)),
    column(2,numericInput("ocost2", value=test.pre[2,"ocost"], step=0.1, label=NA)),
    column(1,textOutput("tcode2"), align="left")
  ),
  fluidRow(
    column(3,textOutput("tname3"), align="left"),
    column(2,numericInput("sen3", value=test.pre[3,"sens"], step=0.01, label=NA)),
    column(2,numericInput("spe3", value=test.pre[3,"spec"], step=0.01, label=NA)),
    column(2,numericInput("dcost3", value=test.pre[3,"dcost"], step=0.1, label=NA)),
    column(2,numericInput("ocost3", value=test.pre[3,"ocost"], step=0.1, label=NA)),
    column(1,textOutput("tcode3"), align="left")
  ),
  fluidRow(
    column(3,textOutput("tname4"), align="left"),
    column(2,numericInput("sen4", value=test.pre[4,"sens"], step=0.01, label=NA)),
    column(2,numericInput("spe4", value=test.pre[4,"spec"], step=0.01, label=NA)),
    column(2,numericInput("dcost4", value=test.pre[4,"dcost"], step=0.1, label=NA)),
    column(2,numericInput("ocost4", value=test.pre[4,"ocost"], step=0.1, label=NA)),
    column(1,textOutput("tcode4"), align="left")
  ),
  fluidRow(
    tags$div(
      style="padding: 10px 0px 10px 15px;",
      h5(tags$b(tags$i("Screening test - Symptom screening")))
    )
  ),
  fluidRow(
    column(3,textOutput("tname5"), align="left"),
    column(2,numericInput("sen5", value=test.pre[5,"sens"], step=0.01, label=NA)),
    column(2,numericInput("spe5", value=test.pre[5,"spec"], step=0.01, label=NA)),
    column(2,numericInput("dcost5", value=test.pre[5,"dcost"], step=0.1, label=NA)),
    column(2,numericInput("ocost5", value=test.pre[5,"ocost"], step=0.1, label=NA)),
    column(1,textOutput("tcode5"), align="left")
  ),
  fluidRow(
    column(3,textOutput("tname6"), align="left"),
    column(2,numericInput("sen6", value=test.pre[6,"sens"], step=0.01, label=NA)),
    column(2,numericInput("spe6", value=test.pre[6,"spec"], step=0.01, label=NA)),
    column(2,numericInput("dcost6", value=test.pre[6,"dcost"], step=0.1, label=NA)),
    column(2,numericInput("ocost6", value=test.pre[6,"ocost"], step=0.1, label=NA)),
    column(1,textOutput("tcode6"), align="left")
  ),
  fluidRow(
    column(3,textOutput("tname7"), align="left"),
    column(2,numericInput("sen7", value=test.pre[7,"sens"], step=0.01, label=NA)),
    column(2,numericInput("spe7", value=test.pre[7,"spec"], step=0.01, label=NA)),
    column(2,numericInput("dcost7", value=test.pre[7,"dcost"], step=0.1, label=NA)),
    column(2,numericInput("ocost7", value=test.pre[7,"ocost"], step=0.1, label=NA)),
    column(1,textOutput("tcode7"), align="left")
  ),
  fluidRow(
    column(3,textOutput("tname8"), align="left"),
    column(2,numericInput("sen8", value=test.pre[8,"sens"], step=0.01, label=NA)),
    column(2,numericInput("spe8", value=test.pre[8,"spec"], step=0.01, label=NA)),
    column(2,numericInput("dcost8", value=test.pre[8,"dcost"], step=0.1, label=NA)),
    column(2,numericInput("ocost8", value=test.pre[8,"ocost"], step=0.1, label=NA)),
    column(1,textOutput("tcode8"), align="left")
  ),
  fluidRow(
    column(3,textOutput("tname9"), align="left"),
    column(2,numericInput("sen9", value=test.pre[9,"sens"], step=0.01, label=NA)),
    column(2,numericInput("spe9", value=test.pre[9,"spec"], step=0.01, label=NA)),
    column(2,numericInput("dcost9", value=test.pre[9,"dcost"], step=0.1, label=NA)),
    column(2,numericInput("ocost9", value=test.pre[9,"ocost"], step=0.1, label=NA)),
    column(1,textOutput("tcode9"), align="left")
  ),
  fluidRow(
    tags$div(
      style="padding: 10px 0px 10px 15px;",
      h5(tags$b(tags$i("Diagnostic test")))
    )
  ),
  fluidRow(
    column(3,textOutput("tname10"), align="left"),
    column(2,numericInput("sen10", value=test.pre[10,"sens"], step=0.01, label=NA)),
    column(2,numericInput("spe10", value=test.pre[10,"spec"], step=0.01, label=NA)),
    column(2,numericInput("dcost10", value=test.pre[10,"dcost"], step=0.1, label=NA)),
    column(2,numericInput("ocost10", value=test.pre[10,"ocost"], step=0.1, label=NA)),
    column(1,textOutput("tcode10"), align="left")
  ),
  fluidRow(
    column(3,textOutput("tname11"), align="left"),
    column(2,numericInput("sen11", value=test.pre[11,"sens"], step=0.01, label=NA)),
    column(2,numericInput("spe11", value=test.pre[11,"spec"], step=0.01, label=NA)),
    column(2,numericInput("dcost11", value=test.pre[11,"dcost"], step=0.1, label=NA)),
    column(2,numericInput("ocost11", value=test.pre[11,"ocost"], step=0.1, label=NA)),
    column(1,textOutput("tcode11"), align="left")
  ),
  fluidRow(
    column(3,textOutput("tname12"), align="left"),
    column(2,numericInput("sen12", value=test.pre[12,"sens"], step=0.01, label=NA)),
    column(2,numericInput("spe12", value=test.pre[12,"spec"], step=0.01, label=NA)),
    column(2,numericInput("dcost12", value=test.pre[12,"dcost"], step=0.1, label=NA)),
    column(2,numericInput("ocost12", value=test.pre[12,"ocost"], step=0.1, label=NA)),
    column(1,textOutput("tcode12"), align="left")
  ),
  #---------------Fukushi - adding three custom tests
  fluidRow(
    tags$div(
      style="padding: 10px 0px 10px 15px;",
      h5(tags$b(tags$i("Custom test")))
    )
  ),
  fluidRow(
    column(3,textInput("ctname13", value=test.pre[13,"desc"], label=NA), align="left"), 
    column(2,numericInput("sen13", value=test.pre[13,"sens"], step=0.01, label=NA)),
    column(2,numericInput("spe13", value=test.pre[13,"spec"], step=0.01, label=NA)),
    column(2,numericInput("dcost13", value=test.pre[13,"dcost"], step=0.1, label=NA)),
    column(2,numericInput("ocost13", value=test.pre[13,"ocost"], step=0.1, label=NA)),
    column(1,textOutput("tcode13"), align="left")
  ),
  fluidRow(
    column(3,textInput("ctname14", value=test.pre[14,"desc"], label=NA), align="left"), 
    column(2,numericInput("sen14", value=test.pre[14,"sens"], step=0.01, label=NA)),
    column(2,numericInput("spe14", value=test.pre[14,"spec"], step=0.01, label=NA)),
    column(2,numericInput("dcost14", value=test.pre[14,"dcost"], step=0.1, label=NA)),
    column(2,numericInput("ocost14", value=test.pre[14,"ocost"], step=0.1, label=NA)),
    column(1,textOutput("tcode14"), align="left")
  ),
  fluidRow(
    column(3,textInput("ctname15", value=test.pre[15,"desc"], label=NA), align="left"), 
    column(2,numericInput("sen15", value=test.pre[15,"sens"], step=0.01, label=NA)),
    column(2,numericInput("spe15", value=test.pre[15,"spec"], step=0.01, label=NA)),
    column(2,numericInput("dcost15", value=test.pre[15,"dcost"], step=0.1, label=NA)),
    column(2,numericInput("ocost15", value=test.pre[15,"ocost"], step=0.1, label=NA)),
    column(1,textOutput("tcode15"), align="left")
  ),
  #---------------
  fluidRow(
    br(),
    br(),
    br(),
    tableOutput("testtable"),
    br()
  )
),

tabPanel(
  "Algorithm",icon = icon("forward"),
  fluidRow(
    tags$div(
      style="padding: 10px 20px 10px 20px;",
      h4("Define a set of algorithms"),
      p("These algorithms are preset according to the WHO guidlines on TB screening. Please refer Chapter 9. Users can change any of these algoithms by selecting screening and diagnotic tools from dropdown menues."),
      br()
    )
  ),
  fluidRow(
    column(3,
           tags$div(
             title="Ten preset algorithms are from the WHO guidelines.",
             strong("Algorithm name",tags$i(class="fa fa-info-circle", style="color:skyblue"))
           )
    ),
    column(3,
           tags$div(
             title="First screening test uses sympton or chest X-ray screening. Please check the Test panel for accuracy of tools. ",
             strong("Screening test 1",tags$i(class="fa fa-info-circle", style="color:skyblue"))
           )
    ),
    column(3,
           tags$div(
             title="Screening step 2 usually employ X-ray to narrowing down subject for diagnostic testing.",
             strong("Screening test 2",tags$i(class="fa fa-info-circle", style="color:skyblue"))
           )
    ),
    column(3,
           tags$div(
             title="Diagnostic tests include sputum smear microscopy, Xpert and clinical diagnosis.",
             strong("Diagnostic test",tags$i(class="fa fa-info-circle", style="color:skyblue"))
           )
    )
  ),
  fluidRow(
    column(3,textInput("algname1u", value=algdf.pre[1,"Alg"], label=NA)),
    column(3,selectInput("s1test1",label=NULL,choices=lstchoi.s1, selected=algdf.pre[1,2])),
    column(3,selectInput("s2test1",label=NULL,choices=lstchoi.s2, selected=algdf.pre[1,3])),
    column(3,selectInput("d1test1",label=NULL,choices=lstchoi.d1, selected=algdf.pre[1,4]))
  ),
  fluidRow(
    column(3,textInput("algname2u", value=algdf.pre[2,"Alg"], label=NA)),
    column(3,selectInput("s1test2",label=NULL,choices=lstchoi.s1, selected=algdf.pre[2,2])),
    column(3,selectInput("s2test2",label=NULL,choices=lstchoi.s2, selected=algdf.pre[2,3])),
    column(3,selectInput("d1test2",label=NULL,choices=lstchoi.d1, selected=algdf.pre[2,4]))
  ),
  fluidRow(
    column(3,textInput("algname3u", value=algdf.pre[3,"Alg"], label=NA)),
    column(3,selectInput("s1test3",label=NULL,choices=lstchoi.s1, selected=algdf.pre[3,2])),
    column(3,selectInput("s2test3",label=NULL,choices=lstchoi.s2, selected=algdf.pre[3,3])),
    column(3,selectInput("d1test3",label=NULL,choices=lstchoi.d1, selected=algdf.pre[3,4]))
  ),
  fluidRow(
    column(3,textInput("algname4u", value=algdf.pre[4,"Alg"], label=NA)),
    column(3,selectInput("s1test4",label=NULL,choices=lstchoi.s1, selected=algdf.pre[4,2])),
    column(3,selectInput("s2test4",label=NULL,choices=lstchoi.s2, selected=algdf.pre[4,3])),
    column(3,selectInput("d1test4",label=NULL,choices=lstchoi.d1, selected=algdf.pre[4,4]))
  ),
  fluidRow(
    column(3,textInput("algname5u", value=algdf.pre[5,"Alg"], label=NA)),
    column(3,selectInput("s1test5",label=NULL,choices=lstchoi.s1, selected=algdf.pre[5,2])),
    column(3,selectInput("s2test5",label=NULL,choices=lstchoi.s2, selected=algdf.pre[5,3])),
    column(3,selectInput("d1test5",label=NULL,choices=lstchoi.d1, selected=algdf.pre[5,4]))
  ),
  fluidRow(
    column(3,textInput("algname6u", value=algdf.pre[6,"Alg"], label=NA)),
    column(3,selectInput("s1test6",label=NULL,choices=lstchoi.s1, selected=algdf.pre[6,2])),
    column(3,selectInput("s2test6",label=NULL,choices=lstchoi.s2, selected=algdf.pre[6,3])),
    column(3,selectInput("d1test6",label=NULL,choices=lstchoi.d1, selected=algdf.pre[6,4]))
  ),
  fluidRow(
    column(3,textInput("algname7u", value=algdf.pre[7,"Alg"], label=NA)),
    column(3,selectInput("s1test7",label=NULL,choices=lstchoi.s1, selected=algdf.pre[7,2])),
    column(3,selectInput("s2test7",label=NULL,choices=lstchoi.s2, selected=algdf.pre[7,3])),
    column(3,selectInput("d1test7",label=NULL,choices=lstchoi.d1, selected=algdf.pre[7,4]))
  ),
  fluidRow(
    column(3,textInput("algname8u", value=algdf.pre[8,"Alg"], label=NA)),
    column(3,selectInput("s1test8",label=NULL,choices=lstchoi.s1, selected=algdf.pre[8,2])),
    column(3,selectInput("s2test8",label=NULL,choices=lstchoi.s2, selected=algdf.pre[8,3])),
    column(3,selectInput("d1test8",label=NULL,choices=lstchoi.d1, selected=algdf.pre[8,4]))
  ),
  fluidRow(
    column(3,textInput("algname9u", value=algdf.pre[9,"Alg"], label=NA)),
    column(3,selectInput("s1test9",label=NULL,choices=lstchoi.s1, selected=algdf.pre[9,2])),
    column(3,selectInput("s2test9",label=NULL,choices=lstchoi.s2, selected=algdf.pre[9,3])),
    column(3,selectInput("d1test9",label=NULL,choices=lstchoi.d1, selected=algdf.pre[9,4]))
  ),
  fluidRow(
    column(3,textInput("algname10u", value=algdf.pre[10,"Alg"], label=NA)),
    column(3,selectInput("s1test10",label=NULL,choices=lstchoi.s1, selected=algdf.pre[10,2])),
    column(3,selectInput("s2test10",label=NULL,choices=lstchoi.s2, selected=algdf.pre[10,3])),
    column(3,selectInput("d1test10",label=NULL,choices=lstchoi.d1, selected=algdf.pre[10,4]))
  ),
  fluidRow(
    column(3,textInput("algname11u", value=algdf.pre[11,"Alg"], label=NA)),
    column(3,selectInput("s1test11",label=NULL,choices=lstchoi.s1, selected=algdf.pre[11,2])),
    column(3,selectInput("s2test11",label=NULL,choices=lstchoi.s2, selected=algdf.pre[11,3])),
    column(3,selectInput("d1test11",label=NULL,choices=lstchoi.d1, selected=algdf.pre[11,4]))
  ),
  fluidRow(
    column(3,textInput("algname12u", value=algdf.pre[12,"Alg"], label=NA)),
    column(3,selectInput("s1test12",label=NULL,choices=lstchoi.s1, selected=algdf.pre[12,2])),
    column(3,selectInput("s2test12",label=NULL,choices=lstchoi.s2, selected=algdf.pre[12,3])),
    column(3,selectInput("d1test12",label=NULL,choices=lstchoi.d1, selected=algdf.pre[12,4]))
  ),
  fluidRow(
    column(3,offset=4,
           tags$div(
             h5("Custom test parameters")
           ))),
  fluidRow(
    column(6,offset=4, tags$div(h6(textOutput("tname13"))))),
  fluidRow(
    column(6,offset=4, tags$div(h6(textOutput("tname14"))))),
  fluidRow(
    column(6,offset=4, tags$div(h6(textOutput("tname15")))),
    
    br(),
    br(),
#    tableOutput("out_algdf"),
    br()
  )
),

#     tabPanel(
#       "Algorithm-fix",icon = icon("forward"),
#       fluidRow(
#         h4("Diagnostic algorithm"),
#         h5("These algorithms are preset according to the WHO guidlines on TB screening. Please refer Chapter 9."),
#         h5(a("Systematic screening for active tuberculosis: principles and recommendations", 
#              href="http://www.who.int/tb/tbscreening/en/",target="_blank")),
#         br()
#       ),
#       fluidRow(plotOutput("algorithm", width = "auto", height = "auto"))
#     ),

    tabPanel(
      "Output table",icon = icon("table"),
      fluidRow(
        tags$div(
          style="padding: 10px 20px 10px 20px;",
          h4("Screening results for all risk groups with all algorithm"),
          p("Estimated results of screening for all specified risk groups with all specified algorithms."),
          br(),
          downloadButton('downloadData', 'Download'),
          h6("Download as a csv file (can be read by Excel)."),
          br(),
          br()
        )
      ),
      tags$div(
        style="padding: 10px 20px 10px 20px;",
        fluidRow(DT::dataTableOutput("summarytable"))
      )
    ),
    tabPanel(
      "Total yield",icon = icon("bar-chart-o"),
      sidebarLayout(
        sidebarPanel(
          downloadButton("down_toty", "Download as PDF"),
          br(),
          br(),
          uiOutput("ui_toty_select"),
          br()
        ),
        mainPanel(
          plotOutput('plot_toty', width = "auto", height = "auto")
        )
      )
    ),
    tabPanel(
      "True vs False Positives",icon = icon("bar-chart-o"),
      sidebarLayout(sidebarPanel(
        downloadButton("down_tpfp", "Download as PDF"),
        br(),
        br(),
        uiOutput("ui_tpfp_select"),
        br()
        ),
                    mainPanel(
                      plotOutput('plot_tpfp', width = "auto", height = "auto")
                    ))
    ),
    tabPanel(
      "Cost per case",icon = icon("bar-chart-o"),
      sidebarLayout(
        sidebarPanel(
          downloadButton("down_cpc_nns", "Download as PDF (Cost per case & NNS)"),
          br(),
          br(),
          uiOutput("ui_cpc_select"),
          br()
        ),
        mainPanel(
          plotOutput('plot_cpc', width = "auto", height = "auto")
        )
      )
    ),
    tabPanel(
      "NNS",icon = icon("bar-chart-o"),    
      sidebarLayout(
        sidebarPanel(
          downloadButton("down_cpc_nns2", "Download as PDF (Cost per case & NNS)"),
          br(),
          br(),
          uiOutput("ui_nns_select"),
          br()
        ),
        mainPanel(
          plotOutput('plot_nns', width = "auto", height = "auto")
        )
      )
    ),
    tabPanel(
      "Cost vs yield",icon = icon("bar-chart-o"),
      sidebarLayout(
        sidebarPanel(
          downloadButton("down_cvy", "Download as PDF"),
          br(),
          br(),
          uiOutput("ui_cvy_select"),
          br()
        ),
        mainPanel(
          plotOutput('plot_cvy', width = "auto", height = "auto")
        )
      )
    ),
    tabPanel(
      "Scenario",icon = icon("book"),
      fluidRow(
        tags$div(
          style="padding: 10px 20px 10px 20px;",
          h4("Scenario management"),
          p("User defined values including country baseline prevalence, risk groups characteristics, test specifications and costs, and diagnostic algorithms can be saved as a single 'scenario file'. Scenario files can be stored for future use, transferred via emails, or widely disseminated so that multiple users can utilize or learn from. 
  This page will also post useful scenarios including country case studies, educational/illustrative examples. 
            "),
          br()
        )
      ),
      sidebarLayout(
        sidebarPanel(width=4,
          h4("Save current parameters"),
          p("The current parameters are showen in the right tables. This dataset can be downloaded as a scenario file and stored in your computer."),
          downloadButton('download_scenariofile', 'Save'),
          br(),
          br(),
          h4("Load scenario file"),
          p("After successful uploading, the previously entered parameters and values are restored and the right tables are updated. Selected checkboxes for the graphic display will be reset back to default settings."),
          fileInput('upload_scenariofile', label=NA, accept = c('.Rdata')),
          #-------------- Fukushi - scenario storage
          h4("Scenario storage"),
          p("Ready-made scenario files can be downloaded and used for exercise."),
          # Scenario 1
          h5("1. Cambodia - introduction of new tools"),
          p("LED-FM, Xpert Ultra and CAD4TB in the custom tests. New algorithms uing Xpert Ultra and CAD4TB. 2011 Prevalence survey result. Default relative risk for risk groups. Pop size adjusted for Cambodia."),
          downloadButton('download_scenario1', 'Save - Scenario 1'),
          # Scenario 2
          h5("2. Cambodia - prioritization of age group"),
          p("TB prevalence of different age groups taken from point estimates of 2011 Prevalence Survey. Pop size taken from Census. Separate files for male, female and all. Tests and algorithms are default."),
          downloadButton('download_scenario2_1', 'Save - Scenario 2.1 (Male)'),
          downloadButton('download_scenario2_2', 'Save - Scenario 2.2 (Female)'),
          downloadButton('download_scenario2_3', 'Save - Scenario 2.3 (All)')
          
          #------------------------------------- until here
          
        ),
        mainPanel(
          h5("Enter informaiton on the current data. This will be saved together within the scenario file."),
          aceEditor("metatxt", value="This scenario is about...",height = "100px", mode="text",
                      theme = "textmate",wordWrap=TRUE,showLineNumbers = FALSE),
          h5("Summary of your current scenario"),
          tableOutput("out_rgrp"),
          tableOutput("out_test"),
          tableOutput("out_algdf"),
#           br(),
#           h4("Data Information"),
#           verbatimTextOutput("datastr"),
          br()
        )
      )
    ),
    tabPanel(
      "About...",icon = icon("comment-o"),
      fluidRow(
        tags$div(
          style="border-style: solid ; border-width: 1px; padding: 10px 10px 10px 10px;
          border-color:lightblue; 
          width:90%; margin-left:auto;margin-right:auto; border-radius: 10px;",
          #background-color: azure;
          p(tags$img(src="WHO-EN-C-H_onlylogo_75.jpg",align="left",hspace="30"),
            "This tool is a part of the WHO guidance on planning and implementation of systematic screening for active TB.",
            br(),
            "It should be used in conjunction with the document",
            a("Systematic screening for active tuberculosis: an operational guide.",href="http://who.int/tb/tbscreening/en/",target="_blank"),
            br(),
            "The operational guide includes an overview of the online tool, describes its limitations, and provides advice on how to use the tool and interpret its outputs.")
        ),
        tags$div(
          style="padding: 10px 20px 10px 20px;",
          h4("Prioritization and planning tool for systematic tuberculosis screening in selected risk groups"),
          p("This tool is designed to aid in the design and prioritization of systematic tuberculosis (TB) screening programs among populations and groups of people at higher risk for TB. The tool uses the best available data in order to generate estimates of the size, yield, and cost of screening programmes, specific to the group targeted for screening and the testing algorithm used."),
          p("The tool is meant to be used as a preliminary prioritization activity, rather than for detailed planning. We encourage users of this tool to repeat the exercise several times, varying some or all of the inputs and the risk groups included, in order to understand how variation in the targeted populations and the screening parameters affects the estimates of size, yield, and cost."),
          br(),
          h4("Contributors"),
          p("Knut Lönnroth, Cecily Miller, Nobuyuki Nishikiori, Fukushi Morishita"),
          br(),
          h4("Help us improve the tool"),
          p("Please provide your feedback and suggestions by using below form.")
        )
      ),
      fluidRow(
        sidebarPanel(
          textInput("name", "Your name:", value=""),
          textInput("from", "Your email:", value=""),
#          textInput("subject", "Subject:", value=""),
          actionButton("send", "Send mail")
        ),
        mainPanel(    
          aceEditor("message", value="write message here",height = "250px", mode="text",
                    theme = "textmate",wordWrap=TRUE,showLineNumbers = FALSE)
        )
      ),
      fluidRow(
        tags$div(
          style="padding: 10px 20px 10px 20px;",
          includeMarkdown("screentbRN.Rmd"),
          br(),
          br()
        )
#       ),
#       fluidRow(
#         p("clientData values"),
#         verbatimTextOutput("clientdataText")
      )
    )
  )
)
