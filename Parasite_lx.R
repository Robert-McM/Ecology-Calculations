##load required dependencies
#library(dplyr)
#library(plyr)
#library(ggplot2)
#library(RColorBrewer)

# input data requires unaggregated cencus dataframe just like what we collected in class
# Names must in this order and as follows LatY, LongX, Leaves, flower, seeds, Parasitized

##### example: parasite_lx(Example_data_for_parasitized_calculations)

Parasite_lx <- function(x) {
  

  #replace NA with zero and create two new dataframes seperated by parasitizism
  x[is.na(x)] <- 0
  no<- x %>% filter(Parasitized == 0)
  yes<- x %>% filter(Parasitized == 1)
  
  #get ride of parasitized status for function to process
  no<- no %>% select(-Parasitized)
  yes<- yes %>% select(-Parasitized)
  
  #aggregate data
  no<- no %>% group_by(LatY, LongX, Leaves) %>% 
    dplyr::summarise(Ax =n(), sum_flower = sum(flower), sum_seed= sum(seeds))
  
  no <- no %>% mutate("bx (seeds/stalk)"= sum_seed/Ax) %>% 
    dplyr::rename("Number of leaves (age) x" = Leaves) %>% 
    dplyr::select(-c(sum_flower, sum_seed))
  
  yes<- yes %>% group_by(LatY, LongX, Leaves) %>% 
    dplyr::summarise(Ax =n(), sum_flower = sum(flower), sum_seed= sum(seeds))
  
  yes <- yes %>% mutate("bx (seeds/stalk)"= sum_seed/Ax) %>% 
    dplyr::rename("Number of leaves (age) x" = Leaves) %>% 
    dplyr::select(-c(sum_flower, sum_seed))
  
  
  #for every unique combination of coordinates
  #split data into seperate groups 
  no<- no %>% group_by(LatY, LongX) %>% group_split()
  yes<- yes %>% group_by(LatY, LongX) %>% group_split()
  
  
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
  }
  
  
  #apply the lifetable function to each group splitted by earlier
  no<- lapply(no, Make_life_table)
  yes<- lapply(yes, Make_life_table)
  
  #Turn the list into dataframe
  no<- plyr::ldply(no, rbind)
  yes<- plyr::ldply(yes, rbind)
  
  #Create treatment status factor variable
  no$Status <- "Parasitized"
  yes$Status <- "Non-parasitized"
  newDF<- rbind(no, yes)
  newDF<- as_tibble(newDF)
  names(newDF)[1]<- "LatY"
  names(newDF)[2]<- "LongX"
  names(newDF)[3]<- "leaves"
  
  #preperation for graphing
  newerDF<- newDF %>% select(LatY, LongX, leaves, lx, Status)
  h<- newerDF %>% filter(Status == "Parasitized")
  j<- newerDF %>% filter(Status == "Non-parasitized")
  
  #plot data
  g<- ggplot()+ 
    geom_line(data = h, aes(x= leaves, y = log(lx), group= interaction(LatY, LongX), color=Status))+
    geom_line(data = j, aes(x= leaves, y = log(lx), group= interaction(LatY, LongX), color=Status))+
    theme_bw()+
    labs(title=expression(paste(italic("Silphium albiflorum "), "Survivorship Curve")), 
                subtitle = "Parasitized vs Non-parasitized",
         x="Number of leaves (age) x", 
         y="ln(lx)")+
    scale_color_brewer(palette = "Accent")
  g
  return(g)
}

  
  
  
