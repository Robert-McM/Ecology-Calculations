##This function requires a single dataframe with the 
#column names year, LatY, LongX, Leaves, flower, and seeds. 
# The year column should be an identifier column and can be generated in R as so
# X2018$year<- 2018
# X2017$year<- 2017
# X2016$year<- 2016
# X2014$year<- 2014
# X2012$year<- 2012

## to make a single dataframe out of all the yearly data do the following
# mydata<- rbind(X2018, X2017, X2016, X2014, X2012)

##Make sure that the data are all numeric, the year column is character, and there
# are no NA values
# str(mydata)
# mydata[,3:5] <- sapply(mydata[,3:5], as.numeric)
# mydata[,6] <- sapply(mydata[,6], as.character)
# mydata[is.na(mydata)] <- 0


##load required dependencies
# library(dplyr)
# library(plyr)
# library(ggplot2)

##example: lx_year(mydata)

lx_year<- function(x) {
    
    
    Make_life_table<- function(x) {
      
      #aggregate data by counting summarizing unique rows of the same combinations of coordinates 
      #and leaves then summing the number of seeds and flowering stalk for those combinations
      x<- x %>% group_by(LatY, LongX, Leaves, year) %>% 
        dplyr::summarise(Ax =n(), sum_flower = sum(flower), sum_seed= sum(seeds))
      
      x <- x %>% mutate("bx (seeds/stalk)"= sum_seed/Ax) %>% 
        dplyr::rename("Number of leaves (age) x" = Leaves) %>% 
        dplyr::select(-c(sum_flower, sum_seed))
      
      x<- as.list(x)
      #Survivorship 
      
      Sx <- sum(x$Ax) - cumsum(x$Ax) + x$Ax
      lx<- c()
      lx[1]<- 1
      
      for(i in 2:length(x$`Number of leaves (age) x`)){
        lx[i] <- Sx[i]/Sx[1]
      }
      
      
      #Reproductive value
      gx<- c()
      `lx_bx`<- c()
      `lx_bx_x`<- c()
      
      for (i in 1:length(x$Ax)) {
        gx[i] <- lx[i+1]/lx[i]
        
        `lx_bx`[i]<- lx[i]*x$`bx (seeds/stalk)`[i]
        
        `lx_bx_x`[i]<- lx[i]*x$`bx (seeds/stalk)`[i]*x$`Number of leaves (age) x`[i]}
      
      Ro<- sum(`lx_bx`)
      G<- sum(`lx_bx_x`/Ro)
      R<- (log(Ro)/G)
      
      #Stable age distribution
      `lx*e^rx`<- c()
      cx<- c()
      `e^(rx)/lx`<- c()
      `e^-rx*lx*bx`<- c()
      `sumofe^-rx*lybx`<- c()
      vx<- c()
      
      
      for (i in 1:length(x$Ax)) {
        `lx*e^rx`[i]<- (lx[i])*exp(-R*x$`Number of leaves (age) x`[i])
        
        cx[i] <- `lx*e^rx`[i]/sum(`lx*e^rx`)
        
        `e^(rx)/lx`[i] <- exp(-R*x$`Number of leaves (age) x`[i])/lx[i]
        
        `e^-rx*lx*bx`[i] <- exp(-R*x$`Number of leaves (age) x`[i]*lx[i]*x$`bx (seeds/stalk)`[i])}
      
      `sumofe^-rx*lybx`<- rev(cumsum(rev(`e^-rx*lx*bx`)))
      
      for (i in 1:length(x$Ax)){
        vx[i]<- `e^(rx)/lx`[i]*`sumofe^-rx*lybx`[i+1]
      }
      
      
      x<- tibble(x$year, x$LatY, x$LongX, x$`Number of leaves (age) x`, x$Ax, x$`bx (seeds/stalk)`, 
                 Sx, lx, gx,`lx_bx`,`lx_bx_x`,
                 `lx*e^rx`, cx, `e^(rx)/lx`,
                 `e^-rx*lx*bx`, `sumofe^-rx*lybx`, vx)
      
      return(x)
    }
    
    #for every unique combination of coordinates
    #split data into seperate groups 
    x<- x %>% group_by(LatY, LongX) %>% group_split()
    
    #apply the lifetable function to each group splitted by earlier
    bb<- lapply(x, Make_life_table)
    
    bb<- plyr::ldply(bb, rbind)
    
    names(bb)[1]<- "year"
    names(bb)[2]<- "LatY"
    names(bb)[3]<- "LongX"
    names(bb)[4]<- "leaves"
    
    newerDF<- bb %>% select(year, LatY, LongX, leaves, lx)
    
    g<- ggplot()+ 
      geom_line(data = newerDF, aes(x= leaves, y = log(lx), group= interaction(LatY, LongX), 
                                    color = year),size=0.2)+
      labs(title = "Silphium Albiflorum Yearly Survivorship Comparison", y= "ln(lx)", 
           x= "Number of Leaves (age) x", 
           subtitle = "Tandy Hills")+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5))+
      theme(legend.position = "none")+
      facet_wrap(~ year)
    
    return(g)

}
  


