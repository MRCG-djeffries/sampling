covidlabels_v2=function(){
  # 8 dec 2020
  # generates the labels allocation for treatments
  # cohort 1 index cases
  # two of A,B,C,D,E,F to placebo
  library(flextable)
  library(officer)
  library(magrittr)
  dum=LETTERS[1:6]
  l1=c('B','D','E','A','C','F')
  for ( i in 1 : 10000){
    set.seed((228161+i))
    if (sum(l1==dum[sample(1:6,6)])==6){
      df = 45
    }
  }
  
  P1=c(rep("Placebo",2),rep("IVM",4)) # arms 1 to 3 index
  P2=c(rep("Placebo",4),rep("IVM",2)) # arms 1 to 3 HH
  P3=c(rep("Placebo",3),rep("IVM",3)) # arms 1 to 3 HH
  arm1=c(1,1,2,2,3,3) # for cohort 1
  arm2=c(1,1,2,2,3,3) # for cohort 2
  set.seed(228188)
  Allocation1=dum[sample(1:6,6)]
  set.seed(228879)
  Allocation2=dum[sample(1:6,6)] 
  
  
  df1=data.frame(TreatmentLabel=Allocation1[1],TreatAllocation=P1[1])
  for ( i in 2 : 6){
    dum = data.frame(TreatmentLabel=Allocation1[i],TreatAllocation=P1[i])
    df1=rbind(df1,dum)
  }
  
  df2=data.frame(TreatmentLabel=Allocation1[1],TreatAllocation=P2[1])
  for ( i in 2 : 6){
    dum = data.frame(TreatmentLabel=Allocation1[i],TreatAllocation=P2[i])
    df2=rbind(df2,dum)
  }
  
  df3=data.frame(TreatmentLabel=Allocation2[1],TreatAllocation=P3[1])
  for ( i in 2 : 6){
    dum = data.frame(TreatmentLabel=Allocation2[i],TreatAllocation=P3[i])
    df3=rbind(df3,dum)
  }
  
  word_export <- read_docx()
  for ( i in 1 : 6){
    FF=write_data_frame(Allocation1,arm1,P1,i,"Cohort 1 - Index Case")
    body_add_flextable(word_export, FF)
    body_add_break(word_export, pos = "after")
    # body_add_par(word_export, value = "")
    # body_add_par(word_export, value = "")
    # body_add_par(word_export, value = "")
    # body_add_par(word_export, value = "")

  }
  for ( i in 7 : 12){
    FF=write_data_frame(Allocation1,arm1,P2,(i-6),"Cohort 1 - Household")
    body_add_flextable(word_export, FF)
    body_add_break(word_export, pos = "after")
    # body_add_par(word_export, value = "")
    # body_add_par(word_export, value = "")
    # if (i !=12){
    # body_add_par(word_export, value = "")
    # body_add_par(word_export, value = "")
    # }
  }
  
  for ( i in 13 : 18){
    FF=write_data_frame(Allocation2,arm2,P3,(i-12),"Cohort 2")
    body_add_flextable(word_export, FF)
    body_add_break(word_export, pos = "after")
    # body_add_par(word_export, value = "")
    # body_add_par(word_export, value = "") 
    # body_add_par(word_export, value = "")
    # body_add_par(word_export, value = "")
    
  }
  df1=flextable(df1)
  df1= set_caption(df1, caption = "Cohort 1 - Index Case")
  df1= autofit(df1)
  body_add_flextable(word_export, df1)
  body_add_par(word_export, value = "")
  df2=flextable(df2)
  df2= set_caption(df2, caption = "Cohort 1 - Household Contacts")
  df2= autofit(df2)
  body_add_flextable(word_export, df2)
  body_add_par(word_export, value = "")
  df3=flextable(df3)
  df3= set_caption(df3, caption = "Cohort  2")
  df3= autofit(df3)
  body_add_flextable(word_export, df3)
  
  print(word_export, "~/covidtrial/rando1.docx")
  
}

  write_data_frame=function(Allocation,arm,P,i,titly){
  df=data.frame(Trial="PaTs",TrialNumber="LEO 22628",Cohort=titly,
                Allocation=Allocation[i],Arm=arm[i],Product=P[i])
  FF=flextable(df)
  FF= set_caption(FF, caption = titly)
  FF=align(FF, i = NULL, j = c(4,5,6), align = "center", part = "all")
  FF= autofit(FF)
  return(FF)
}
