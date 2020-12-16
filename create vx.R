##load required dependencies
# library(dplyr)
# library(ggplot2)
# this function works with data generated in class with slight modifications to the column names
# requires a dataframe with column titles LatY, 
# LongX, Leaves, flower, seeds
# example: Create_vx_curve(mydata)

##in the event that an error occurs that column in not numeric such as the seeds column either 
# multiply the entire column by 1 in excel and re-import or do the following in R
# mydata$seeds <- as.numeric(as.character(mydata$seeds))


Create_vx_curve<- function(x) {
  
  #aggregate data by counting summarizing unique rows of the same combinations of coordinates 
  #and leaves then summing the number of seeds and flowering stalk for those combinations
  x<- x %>% group_by(LatY, LongX, Leaves) %>% 
    dplyr::summarise(Ax =n(), sum_flower = sum(flower), sum_seed= sum(seeds))
  
  x <- x %>% mutate("bx (seeds/stalk)"= sum_seed/Ax) %>% 
    dplyr::rename("Number of leaves (age) x" = Leaves) %>% 
    dplyr::select(-c(sum_flower, sum_seed))
  
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
  
  #for every unique combination of coordinates
  #split data into seperate groups 
  x<- x %>% group_by(LatY, LongX) %>% group_split()
  
  #apply the lifetable function to each group splitted by earlier
  bb<- lapply(x, Make_life_table)
  
  
  #graphing begins here
  my_names = c("gx", "vx")
  
  
  result = lapply(bb, "[", , my_names)
  
  new_col_name <- c("x", "y")
  result<- lapply(result, setNames, nm = new_col_name)
  
  g <- ggplot()
  for (i in 1:length(result)) g <- g + geom_line(data=result[[i]], aes(x,log(y), col=y), 
                                                 size=.2)+ 
    labs(title = "Silphium Albiflorum Trade-off Between Current and Future Reproduction", 
         y= "Residual Reproductive Value ln(vx)", x= "Current Reproductive Effort (gx)")+
    scale_color_gradient(low="#00BFC4", high="#F8766D")+
    theme_bw()+ 
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5))
  
  return (g)
  
  
}

