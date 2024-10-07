###this R code manuscript is for the presentation results demonstration, as well as for the paper writing

##this session is for the variable statistics output
{
  top<-c("東京二十三区","大阪市","名古屋市","横浜市")
  cdf<-variable_statsitics
#  View(cdf)
  vs<-c("work","trans","leisure","resi")
  result<-data.frame()
  h=1
  for (year in years) {
    for (variable in vs) {
    result[h,"Group"]<-"Top"
    result[h,"var"]<-variable
    result[h,"year"]<-year
    result[h,"mean"]<-mean(cdf$mean[cdf$var==variable&cdf$city%in%top&cdf$year==year])
    result[h,"Moran"]<-mean(cdf$moran[cdf$var==variable&cdf$city%in%top&cdf$year==year])
    result[h,"hhi"]<-mean(cdf$hhi[cdf$var==variable&cdf$city%in%top&cdf$year==year])
    result[h,"ineq"]<-mean(cdf$ineq[cdf$var==variable&cdf$city%in%top&cdf$year==year])
    h=h+1
    result[h,"Group"]<-"Other"
    result[h,"var"]<-variable
    result[h,"year"]<-year
    result[h,"mean"]<-mean(cdf$mean[cdf$var==variable&!cdf$city%in%top&cdf$year==year])
    result[h,"Moran"]<-mean(cdf$moran[cdf$var==variable&!cdf$city%in%top&cdf$year==year])
    result[h,"hhi"]<-mean(cdf$hhi[cdf$var==variable&!cdf$city%in%top&cdf$year==year])
    result[h,"ineq"]<-mean(cdf$ineq[cdf$var==variable&!cdf$city%in%top&cdf$year==year])
    h=h+1
    result[h,"Group"]<-"Average"
    result[h,"var"]<-variable
    result[h,"year"]<-year
    result[h,"mean"]<-mean(cdf$mean[cdf$var==variable&cdf$year==year])
    result[h,"Moran"]<-mean(cdf$moran[cdf$var==variable&cdf$year==year])
    result[h,"hhi"]<-mean(cdf$hhi[cdf$var==variable&cdf$year==year])
    result[h,"ineq"]<-mean(cdf$ineq[cdf$var==variable&cdf$year==year])
    h=h+1
    }
    }
}
variable_summary<-result
write.csv(variable_summary,"grouped_summary.csv")


###Shapley decomposition
##importance of each variable for explaining the variety
{
  cdf<-do.call(rbind, combined_data)
  
  symbol<-"Q"
  cdf$housing<-rowSums(cdf[,c(paste0(symbol,c(3,21)))])
  #cdf$trans<-rowSums(cdf[,c(paste0(symbol,c(22,23,24)))])
  cdf$trans<-cdf[,c(paste0("D",c(22)))]
  cdf$leisure<-rowSums(cdf[,c(paste0(symbol,c(29,31,32,34)))])
  cdf$consum<-rowSums(cdf[,c(paste0(symbol,c(30,34,36)))])
  cdf$resi<-rowSums(cdf[,c(paste0(symbol,c(21,35,37,33)))])
  cdf$work<-rowSums(cdf[,c(paste0(symbol,c(20,19,28,38)))])
  cdf$D22
  #plot(cdf$D22,cdf$work)
  #abline(cdf$D22,cdf$resi)
  
  combined_save<-split(cdf[,c("lon","lat","pop","housing","trans","leisure","consum","resi","work","year")],cdf$urban)
  #landvar<-c("housing","leisure","consum","resi","work","trans")
  landvar<-c("leisure","resi","trans","work")
  #variables<-c("housing","leisure","consum","resi","work","lon","lat")
  city<-"仙台市"
  year<-16
  h=1
  calculation<-data.frame()
  for (city in cl) {
    cdf<-combined_save
    scdf<-cdf[[city]]
    for (year in years) {
      svdf <- scdf[scdf$year == year, ]
      
      for (i in 1:length(landvar)) {
        variable<-landvar[i]  
        spv<-landvar[-i]
        spset<-power_set(spv)
        for (j in 1:length(spset)) {
          set<-spset[[j]]
          regvar<-append(variable,set)
          formula<-paste0("pop~",regvar)
          svformula1<-formula(paste("pop~" , paste(regvar,collapse="+")))
          r1<-summary(lm(svformula1,data=svdf))$r.squared
          if (length(set)==0) {
            r2<-0
          } else {
            svformula2<-formula(paste("pop~" , paste(set,collapse="+")))
            r2<-summary(lm(svformula2,data=svdf))$r.squared
          }
          calculation[h,"r1"]<-r1
          calculation[h,"r2"]<-r2
          calculation[h,"weight"]<-factorial(length(set))*
            factorial(length(landvar)-length(set)-1)/factorial(length(landvar))
          calculation[h,"contribution"]<-r1-r2
          calculation[h,"var"]<-variable
          calculation[h,"control"]<-paste0(set,collapse = "-")
          calculation[h,"year"]<-year
          calculation[h,"city"]<-city
          h=h+1
        }
      }
    }
    
  }
  mean(SVtable$R2[SVtable$year==22])
  SVtable<-aggregate((contribution*weight)~city+year+var,data=calculation,FUN = sum)
  names(SVtable)[length(SVtable)]<-"SV"
  R2sum<-aggregate((contribution*weight)~city+year,data=calculation,FUN = sum)
  names(R2sum)[length(R2sum)]<-"R2"
  SVtable<-merge(SVtable,R2sum,by=c("city","year"))
  SVtable$ratio<-SVtable$SV/SVtable$R2
  plotdata<-data.frame()
  variables<-unique(SVtable$var)
  for (year in years) {
    cdf<-SVtable[SVtable$year==year,]
    for (variable in variables) {
      plotdata[h,"var"]<-variable
      plotdata[h,"year"]<-year
      plotdata[h,"ratio"]<-mean(cdf$ratio[cdf$var==variable])
      h=h+1
    }
  }
  #View(plotdata)
  #View(SVtable)
  variable<-"resi"
  top<-c("東京二十三区","大阪市","名古屋市","横浜市")

  mean(SVtable$SV[SVtable$var==variable&SVtable$year==16&!SVtable$city%in%top])
  ggplot(data=SVtable[SVtable$var==variable&SVtable$city%in%top,],aes(x=as.numeric(year),y=ratio,,color=city))+
    geom_point()+geom_line()+labs(x = "Year", y = variable, title = "Shapley decomposition")
  
  top<-c("東京二十三区","大阪市","名古屋市","横浜市")
  cdf<-SVtable
  #  View(cdf)
  vs<-c("work","trans","leisure","resi")
  result<-data.frame()
  h=1
  for (year in years) {
    for (variable in vs) {
      result[h,"Group"]<-"Top"
      result[h,"var"]<-variable
      result[h,"year"]<-year
      result[h,"SV"]<-mean(cdf$SV[cdf$var==variable&cdf$city%in%top&cdf$year==year])
      result[h,"ratio"]<-mean(cdf$ratio[cdf$var==variable&cdf$city%in%top&cdf$year==year])
      h=h+1
      result[h,"Group"]<-"Other"
      result[h,"var"]<-variable
      result[h,"year"]<-year
      result[h,"SV"]<-mean(cdf$SV[cdf$var==variable&!cdf$city%in%top&cdf$year==year])
      result[h,"ratio"]<-mean(cdf$ratio[cdf$var==variable&!cdf$city%in%top&cdf$year==year])
      h=h+1
      result[h,"Group"]<-"Average"
      result[h,"var"]<-variable
      result[h,"year"]<-year
      result[h,"SV"]<-mean(cdf$SV[cdf$var==variable&cdf$year==year])
      result[h,"ratio"]<-mean(cdf$ratio[cdf$var==variable&cdf$year==year])
      h=h+1
    }
  }
  SD_summary<-result
  plotdata<-SD_summary
  Shapley_Graph<-list()
  plot_SD_top<-ggplot(data=plotdata[plotdata$Group=="Top",],aes(x=as.numeric(year),y=ratio,group=var,colour = var,shape = var))+
    geom_point()+geom_line()+labs(x="Year",y="Ratio",title="Shapley decomposition")
  plot_SD_top
  ggsave(filename = paste0("SD_average", ".png"), plot = plot_SD_average)
  Shapley_Graph["Other"]<-plot_SD_other

  
  View(SVtable)
  landuse
  plotdata<-SVtable[SVtable$var=="resi",]
  plotdata<-aggregate(SV~var+year,data=SVtable,FUN=mean)
  ggplot(data=plotdata,aes(x=as.numeric(year),y=SV,group=var,colour = var,shape = var))+geom_point()+geom_line()+labs(x="Year",y="Ratio",title="Shapley decomposition")
  sum(SVtable$SV[SVtable$city=="東京二十三区"&SVtable$year==16])
  summary(combined_data)
  
  plotdata<-aggregate(SV~var+year,data=SVtable,FUN=mean)
  ggplot(data=plotdata,aes(x=as.numeric(year),y=SV,group=var,color=var))+geom_point()+geom_line()
  
  
}

##From here is the analysis of the coefficient analysis, including significance and coefficient estimates, and the corresponding graph plotting
{
  cdf<-coef_landuse_nocoor
  #View(cdf)
  result<-data.frame()
  h=1
  for (year in years) {
    for (variable in vs) {
      result[h,"Group"]<-"Top"
      result[h,"var"]<-variable
      result[h,"year"]<-year
      result[h,"coefficient"]<-mean(cdf$mean[cdf$var==variable&cdf$city%in%top&cdf$year==year])
      result[h,"significance"]<-mean(cdf$significance[cdf$var==variable&cdf$city%in%top&cdf$year==year])
      h=h+1
      result[h,"Group"]<-"Other"
      result[h,"var"]<-variable
      result[h,"year"]<-year
      result[h,"coefficient"]<-mean(cdf$mean[cdf$var==variable&!cdf$city%in%top&cdf$year==year])
      result[h,"significance"]<-mean(cdf$significance[cdf$var==variable&!cdf$city%in%top&cdf$year==year])
      h=h+1
      result[h,"Group"]<-"Average"
      result[h,"var"]<-variable
      result[h,"year"]<-year
      result[h,"coefficient"]<-mean(cdf$mean[cdf$var==variable&cdf$year==year])
      result[h,"significance"]<-mean(cdf$significance[cdf$var==variable&cdf$year==year])
      h=h+1
    }
  }
  coefficient_grouped<-result
  plotdata<-coefficient_grouped
for (variable in vs) {
  plotname<-paste0("plot_CE_",variable)
  assign(plotname,ggplot(data=plotdata[plotdata$var==variable,],aes(x=as.numeric(year),y=coefficient,group=Group,colour = Group,shape = Group))+
    geom_point()+geom_line()+labs(x="Year",y="Estimates"))
  ggsave(filename = paste0("CE",variable, ".png"), plot = get(plotname))
}
  for (variable in vs) {
    plotname<-paste0("plot_SE_",variable)
    assign(plotname,ggplot(data=plotdata[plotdata$var==variable,],aes(x=as.numeric(year),y=significance,group=Group,colour = Group,shape = Group))+
             geom_point()+geom_line()+labs(x="Year",y="Estimates"))
    ggsave(filename = paste0("SE",variable, ".png"), plot = get(plotname))
  }
  
}

###From here, I redo the one-to-one match between GWR in different years, and calculate the z statsitics which indicate whether this change
##is significant or not.
{
  cdf<-gwr_landuse_nocoor
  yearcompare<-c("1619","1920","2022","1922")
  year<-1619
  
  for (year in yearcompare) {
    yearpair<-c(substr(year, 1, 2),substr(year, 3, 4))
    
    intersection<-intersect(get(paste0("city",yearpair[1])),get(paste0("city",yearpair[2])))
    citylist1<-paste0(intersection,"_",yearpair[1])
    citylist2<-paste0(intersection,"_",yearpair[2])
    #city<-1
    outcome<-list()
    for (city in intersection) {
      city1<-paste0(city,"_",yearpair[1])
      city2<-paste0(city,"_",yearpair[2])
      cdf1<-cdf[[city1]]$SDF
      
      cdf1<-data.frame(cdf1[,!names(cdf1)%in%c("lon","lat")])
      cdf2<-cdf[[city2]]$SDF
      cdf2<-data.frame(cdf2[,!names(cdf2)%in%c("lon","lat")])
      coord1<-cdf1[,c("lon","lat")]
      coord2<-cdf2[,c("lon","lat")]
      names(cdf1)<-paste0(names(cdf1),"_",yearpair[1])
      names(cdf2)<-paste0(names(cdf2),"_",yearpair[2])
      names(cdf1)
      
      #coname1<-c(paste0("lon.1_", yearpair[1]), paste0("lat.1_", yearpair[1]))
      #coname2<-c(paste0("lon.1_", yearpair[2]), paste0("lat.1_", yearpair[2]))
      #mergedata<-merge(cdf1, cdf2, 
      #                   by.x = c(paste0("lon.1_", yearpair[1]), paste0("lat.1_", yearpair[1])), 
      #                   by.y = c(paste0("lon.1_", yearpair[2]), paste0("lat.1_", yearpair[2])))
      mergedata<-merge(cdf1, cdf2, 
                       by.x = c(paste0("lon_", yearpair[1]), paste0("lat_", yearpair[1])), 
                       by.y = c(paste0("lon_", yearpair[2]), paste0("lat_", yearpair[2])))
      varname<-vs
      #varname<-coef_list[[city]]
      for (variable in varname) {
        vecname<-paste0("ratio_",variable)
        vecname1<-paste0(variable,"_",yearpair[1])
        vecname2<-paste0(variable,"_",yearpair[2])
        mergedata[,vecname]<-mergedata[,vecname2]/mergedata[,vecname1]
        z<-(mergedata[,vecname1]-mergedata[,vecname2])/sqrt(mergedata[,paste0(variable,"_SE_",yearpair[1])]^2+
                                                              mergedata[,paste0(variable,"_SE_",yearpair[2])]^2)
        vecname<-paste0("zdiff_",variable)
        mergedata[,vecname]<-z
        vecname<-paste0("p_",variable)
        mergedata[,vecname]<-2 * (1 - pnorm(abs(z)))
        names(mergedata)[1:2]<-c("lon","lat")
      }
      mergedata$city<-city
      outcome[[city]]<-mergedata
    }
    names(mergedata)
    
    #variable<-varname[1]
    assign(paste0("coef_compare",yearpair[1],yearpair[2]),outcome)
  }
  #transform into dataframe
coef_compare1619<-do.call(rbind,coef_compare1619)
coef_compare1920<-do.call(rbind,coef_compare1920)
coef_compare1922<-do.call(rbind,coef_compare1922)  
coef_compare2022<-do.call(rbind,coef_compare2022)

result<-data.frame()
h=1
for (year in yearcompare) {
cdf<-get(paste0("coef_compare",year))  

for (city in cl) {
  scdf<-cdf[cdf$city==city,]
  for (variable in vs) {
    varname<-paste0("zdiff_",variable)
    result[h,"var"]<-variable
    result[h,"city"]<-city
    result[h,"year"]<-year
    names(scdf)
    result[h,"zdiff"]<-mean(scdf[[varname]])
    varname<-paste0("p_",variable)
    result[h,"significance"]<-sum(abs(scdf[[varname]])<0.2)/length(scdf[,1])
    coords<-scdf[,c("lon","lat")]
    knn<-knearneigh(coords, k=8)
    nb <- knn2nb(knn)
    lw <- nb2listw(nb, style = "W")
    moran_result <- moran.test(scdf[,paste0("zdiff_",variable)], lw)
    result[h,"moran"]<-moran_result$estimate[1]
    h=h+1
  }
}

}
compare_summary<-result
###接下来为了作图做准备
cdf<-compare_summary
result<-data.frame()
h=1
for (year in yearcompare) {
  for (variable in vs) {
    result[h,"Group"]<-"Top"
    result[h,"var"]<-variable
    result[h,"year"]<-year
    result[h,"zdiff"]<-mean(cdf$zdiff[cdf$var==variable&cdf$city%in%top&cdf$year==year])
    result[h,"significance"]<-mean(cdf$significance[cdf$var==variable&cdf$city%in%top&cdf$year==year])
    result[h,"moran"]<-mean(cdf$moran[cdf$var==variable&!cdf$city%in%top&cdf$year==year])
    h=h+1
    result[h,"Group"]<-"Other"
    result[h,"var"]<-variable
    result[h,"year"]<-year
    result[h,"zdiff"]<-mean(cdf$zdiff[cdf$var==variable&!cdf$city%in%top&cdf$year==year])
    result[h,"significance"]<-mean(cdf$significance[cdf$var==variable&!cdf$city%in%top&cdf$year==year])
    result[h,"moran"]<-mean(cdf$moran[cdf$var==variable&!cdf$city%in%top&cdf$year==year])
    h=h+1
    result[h,"Group"]<-"Average"
    result[h,"var"]<-variable
    result[h,"year"]<-year
    result[h,"zdiff"]<-mean(cdf$zdiff[cdf$var==variable&cdf$year==year])
    result[h,"significance"]<-mean(cdf$significance[cdf$var==variable&cdf$year==year])
    result[h,"moran"]<-mean(cdf$moran[cdf$var==variable&cdf$year==year])
    h=h+1
  }
}
grouped_coef_compare<-result
plotdata<-grouped_coef_compare
plotdata<-plotdata[plotdata$year%in%c(1619,1920,2022),]
names(plotdata)
for (variable in vs) {
  plotname<-paste0("plot_zdiff_",variable)
  assign(plotname,ggplot(data=plotdata[plotdata$var==variable,],aes(x=as.character(year),y=significance,group=Group,colour = Group,shape = Group))+
           geom_point()+geom_line()+labs(x="Year",y="Estimates"))
  ggsave(filename = paste0("spatial_significance",variable, ".png"), plot = get(plotname))
  get(plotname)
}
write.csv(grouped_coef_compare,"coef_compare_grouped.csv")



result<-data.frame()
cdf<-compare_summary
h=1
groups<-c("Average","Top","Other")
for (group in groups) {
  for (variable in vs) {
    result[h,"group"]<-Average
  }
}
}
pnorm(1.56)
