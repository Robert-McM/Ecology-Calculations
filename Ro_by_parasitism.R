##load required dependencies
# library(dplyr)
# library(plyr)
# library(ggplot2)
# input data requires dataframe with column 
# names in this order and as follows LatY, LongX, Leaves, flower, seeds, Parasitized
# example: Ro_by_parasitism(Example_data_for_parasitized_calculations)


Ro_by_parasitism<- function(x) {
  #replace NA with zero and create two new dataframes seperated by parasitism
  x[is.na(x)] <- 0
  no<- x %>% filter(Parasitized == 0)
  yes<- x %>% filter(Parasitized == 1)
  
  
  #aggregate data by counting summarizing unique rows of the same combinations of coordinates 
  #and leaves then summing the number of seeds and flowering stalk for those combinations
  no<- no %>% group_by(LatY, LongX, Leaves) %>% 
    dplyr::summarise(Ax =n(), sum_flower = sum(flower), sum_seed= sum(seeds))
  yes<- yes %>% group_by(LatY, LongX, Leaves) %>% 
    dplyr::summarise(Ax =n(), sum_flower = sum(flower), sum_seed= sum(seeds))
  
  no <- no %>% mutate("bx (seeds/stalk)"= sum_seed/Ax) %>% 
    dplyr::rename("Number of leaves (age) x" = Leaves) %>% 
    dplyr::select(-c(sum_flower, sum_seed))
  
  yes <- yes %>% mutate("bx (seeds/stalk)"= sum_seed/Ax) %>% 
    dplyr::rename("Number of leaves (age) x" = Leaves) %>% 
    dplyr::select(-c(sum_flower, sum_seed))
  
  #for every unique combination of coordinates
  #split data into seperate groups 
  yes<- yes %>% group_by(LatY, LongX) %>% group_split()
  no<- no %>% group_by(LatY, LongX) %>% group_split()
  
  
  #this is the function that makes the lifetable
  Make_life_table<- function(x){
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
    
    x<- tibble(x$LatY, x$LongX, x$`Number of leaves (age) x`, x$Ax, x$`bx (seeds/stalk)`, 
               Sx, lx, gx,`lx_bx`,`lx_bx_x`,
               `lx*e^rx`, cx, `e^(rx)/lx`,
               `e^-rx*lx*bx`, `sumofe^-rx*lybx`, vx)
    return(Ro)
  }
  
  #apply the lifetable function to each group splitted by earlier
  bb<- lapply(yes, Make_life_table)
  
  bn<- lapply(no, Make_life_table)
  
  
  bb<- plyr::ldply(bb, rbind)
  
  bn<- plyr::ldply(bn, rbind)
  
  #label dataframe by treatment status
  bb$Status <- "Parasitized"
  bn$Status <- "Non-parasitized"
  
  newDF<- rbind(bb, bn)
  newDF<- as_tibble(newDF)
  
  names(newDF)[1]<- "Ro"
  
  g<- ggplot(data = newDF, aes(x= Status, y= Ro, color = Status))+ 
    geom_boxplot()+
    geom_jitter()+
    labs(title = "Net Reproductive Value Comparison Parasitized vs Non-parasitized", 
         y = "Net Reproductive Value (Ro)", 
         x= "Parasitism Status")
  return(g)
}

