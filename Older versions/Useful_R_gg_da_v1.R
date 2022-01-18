# ===========================================================================
# Useful R codes for ggplot2 and data analysis
# Version 1
# Created on 10.20.2021 by Rie Sadohara
# ===========================================================================

# Load necessary packages and dataset (in ggplot2).
  library(ggplot2)
  library(dplyr)
  data(mpg) # built within ggplot2 

# Show the first 6 rows of 'mpg' dataset.
  head(mpg)

# View each column (variables) and their types 
  str(mpg)

# check the number of rows (observations).
  nrow(mpg)

# Make some variables as factors to create plots.  
  mpg$cyl_f  = factor(mpg$cyl)
  mpg$year_f = factor(mpg$year)

# See each column (variables) and their types again. The factors are added. 
  str(mpg)

# ===========================================================================
# Remove the inner grid of your plot. 
  mpg %>% 
    ggplot(aes( x=cty, y=hwy )) +
    geom_point() +
    theme_bw(base_size=15) +
    theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
# ===========================================================================

# ===========================================================================
 # Increase the margin between axis title and axis. 
  mpg %>% 
    ggplot(aes( x=cty, y=hwy )) +
    geom_point() +
    theme_bw(base_size=15) +
    theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
    theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) 
# ===========================================================================

# ===========================================================================
# Hide the entire legend.
  mpg %>%
    filter( !is.na(year_f) ) %>%  # Pick up only those that are not NA as X axis.
    ggplot( aes(x=    year_f , 
                y=    cty , 
                fill= year_f ) ) +
    theme_bw(base_size = 15) +
    geom_boxplot(outlier.shape = NA) +  # Hide outliers if using jitter to avoid duplicated dots.
    geom_jitter(color="black", width=0.2) +
    ggtitle("My title" ) +
    theme(legend.position = "none") +
    labs(x=element_blank(), y="City (mpg)") 
# ===========================================================================

# ===========================================================================
# Change the legend title and text.
  # Create a vector that has color names to be used.
  # (color names need to be specified in order to edit the legend text)
  colornames = c("cyan", "magenta")
  
  mpg %>%
    filter( !is.na(year_f) ) %>%  # Pick up only values that are not NA.
    ggplot( aes(x=    year_f , 
                y=    cty , 
                fill= year_f ) ) +
    theme_bw(base_size = 15) +
    geom_boxplot(outlier.shape = NA) +  # Hide outliers if using jitter to avoid duplicated dots.
    geom_jitter(color="black", width=0.2) +
    ggtitle("My title") +
    labs(x=element_blank(), y="City (mpg)", fill = "Edited factor names") +
    theme(axis.text.x = element_blank()) +
    scale_fill_manual(values = colornames, 
                      labels = c("1999 (Year 1)", "2008 (Year 9)") ) 
# ===========================================================================

# ===========================================================================
# Change the factor names on the x axis of the plot.
  mpg %>%
  filter( !is.na(year_f) ) %>%  # Pick up only those that are not NA as X axis.
  ggplot( aes(x=    year_f , 
              y=    cty , 
              fill= year_f ) ) +
  theme_bw(base_size = 15) +
  geom_boxplot(outlier.shape = NA) +  # Hide outliers if using jitter to avoid duplicated dots.
  geom_jitter(color="black", width=0.2) +
  ggtitle("My title" ) +
  theme(legend.position = "none") +
  labs(x=element_blank(), y="City (mpg)") +
  scale_x_discrete(labels = c("Year 1999", "Year 2008"))
# ===========================================================================

# ===========================================================================
# Sort by median and make a boxplot.
  # Need "forcats" package.
  library(forcats)
  
  # Convert "drv" to a factor.
  mpg$drv_f = factor(mpg$drv)
  
  mpg %>% 
    ggplot( aes(x= fct_reorder(drv_f, cty, .desc = F), 
                y = cty, 
                fill = fct_reorder(drv_f, cty, .desc = F))) +
    theme_bw(base_size = 15) +
    geom_boxplot(outlier.shape = NA) +  # Hide outliers if using jitter to avoid duplicated dots.
    geom_jitter(color="black", width=0.2) +
    ggtitle("My title" ) + 
    labs(x=element_blank(), y="City (mpg)", fill = "Drive")
# ===========================================================================

# ===========================================================================
  # Add a rectangular shape and a horizontal line to your chart.
  mpg %>% 
    ggplot( aes(x= fct_reorder(drv_f, cty, .desc = F), 
                y = cty, 
                fill = fct_reorder(drv_f, cty, .desc = F))) +
    geom_rect( aes(xmin = -Inf, xmax = Inf, ymin = 15, ymax = 25), fill = "grey90") +
    geom_hline(yintercept = 20, color="grey48", size=1, linetype=1) +  
    theme_bw(base_size = 15) +
    geom_boxplot(outlier.shape = NA) +  # Hide outliers if using jitter to avoid duplicated dots.
    geom_jitter(color="black", width=0.2) +
    ggtitle("My title" ) + 
    labs(x=element_blank(), y="City (mpg)", fill = "Drive") 
    
# ===========================================================================

# ===========================================================================
# Add a y=x reference line in your chart.
  mpg %>% 
    ggplot( aes(x = cty, y = hwy) ) +
    geom_abline(slope = 1, intercept = 0, color="grey48", linetype = 2) +  
    theme_bw(base_size = 15) +
    geom_point(size = 2, na.rm = F, shape = 4) + 
    ggtitle("My title" ) + 
    labs(x = "Highway (mpg)", y="City (mpg)") +
    xlim(5, 50) + ylim(5, 50) +
    theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0) ) ) +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0) ) ) 
# ===========================================================================  

# ===========================================================================
 # Set axis limits with custom breaks.
 mpg %>% 
   ggplot( aes(x = cty, y = hwy) ) +
   theme_bw(base_size = 15) +
   geom_point(size = 2, na.rm = F, shape = 4) + 
   ggtitle("My title" ) + 
   labs(x = "Highway (mpg)", y="City (mpg)") +
   scale_y_continuous( breaks= round( seq(from=5, to=50, by=4), 0 ), limits = c(5, 50) ) +
   theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
   theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0) ) ) +
   theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0) ) )  
# ===========================================================================

# ===========================================================================
 # Scatter plot, black outlined circles filled with gradient color of another variable. 
   # Calculate the mean value of the variable to be used for coloring 
   mid_displ = mean(mpg$displ, na.rm=T)
 
   mpg %>%
     ggplot(aes(x=cty, y=hwy ) ) + 
     geom_point(size=3, na.rm=T, shape=21, aes(fill=displ), color="black") +
     scale_fill_gradient2(midpoint=mid_displ, low="royalblue3", mid="yellow", high="firebrick3" ) +
     theme_bw(base_size = 15) +
     labs(x="Highway (mpg)", y="City (mpg)") 
# ===========================================================================

# ===========================================================================
# Fix the aspect ratio of a plot regardless of the plot window size of your R studio.
 # Aspect ratio of 0.9 
 mpg %>%
   ggplot(aes(x=cty, y=hwy ) ) + 
   geom_point(size=3, na.rm=T, shape=21, 
              aes(fill=displ), 
              color="black") +
   scale_fill_gradient2(midpoint=mid_displ, 
                        low="royalblue3", 
                        mid="yellow", 
                        high="firebrick3" ) +
   theme_bw(base_size = 15) +
   labs(x="Highway (mpg)", y="City (mpg)") +
   theme(aspect.ratio = 0.9)
 
 # Aspect ratio of 0.6
 mpg %>%
   ggplot(aes(x=cty, y=hwy ) ) + 
   geom_point(size=3, na.rm=T, shape=21, 
              aes(fill=displ), 
              color="black") +
   scale_fill_gradient2(midpoint=mid_displ, 
                        low="royalblue3", 
                        mid="yellow", 
                        high="firebrick3" ) +
   theme_bw(base_size = 15) +
   labs(x="Highway (mpg)", y="City (mpg)") +
   theme(aspect.ratio = 0.6)
# ===========================================================================

# ===========================================================================
 # Save charts with a consistent size.
 # Specify aspect ratio in ggplot code  AND  Specify plot size in ggsave code.
 myplot1 = mpg %>%
   ggplot(aes(x=cty, y=hwy ) ) + 
   geom_point(size=3, na.rm=T, shape=21, aes(fill=displ), color="black") +
   scale_fill_gradient2(midpoint=mid_displ, low="royalblue3", mid="yellow", high="firebrick3" ) +
   theme_bw(base_size = 15) +
   labs(x="Highway (mpg)", y="City (mpg)")  +
   theme(aspect.ratio = 0.9)
 myplot1
 
 # Save your chart as a tiff file in your working directory.
 ggsave("Mysavedplot1.tif", myplot1, device='tiff', width=7, height=4, dpi=200) 
 
 # Show the current working directory
 getwd()
# ===========================================================================

# ===========================================================================
# Organize multiple plots in one chart.
  # Plot highway and city millage of Dodge vehicles
  myplot_dodge = mpg %>% filter(manufacturer=="dodge") %>% 
   ggplot(aes(x=cty, y=hwy ) ) + 
   geom_point(size=3, na.rm=T, shape=21, color="black") +
   theme_bw(base_size = 13) +
   labs(x="Highway (mpg)", y="City (mpg)", title="Dodge")  +
   theme(aspect.ratio = 0.9)
  myplot_dodge
  
  # Plot highway and city millage of Ford vehicles
  myplot_ford = mpg %>% filter(manufacturer=="ford") %>% 
   ggplot(aes(x=cty, y=hwy ) ) + 
   geom_point(size=3, na.rm=T, shape=21, color="black") +
   theme_bw(base_size = 13) +
   labs(x="Highway (mpg)", y="City (mpg)", title="Ford")  +
   theme(aspect.ratio = 0.9)
  myplot_ford
  
  # Plot highway and city millage of Toyota vehicles
  myplot_toyota = mpg %>% filter(manufacturer=="toyota") %>% 
    ggplot(aes(x=cty, y=hwy ) ) + 
    geom_point(size=3, na.rm=T, shape=21, color="black") +
    theme_bw(base_size = 13) +
    labs(x="Highway (mpg)", y="City (mpg)", title="Toyota")  +
    theme(aspect.ratio = 0.9)
  myplot_toyota
  
  # Plot highway and city millage of Volkswagen vehicles
  myplot_volks = mpg %>% filter(manufacturer=="volkswagen") %>% 
    ggplot(aes(x=cty, y=hwy ) ) + 
    geom_point(size=3, na.rm=T, shape=21, color="black") +
    theme_bw(base_size = 13) +
    labs(x="Highway (mpg)", y="City (mpg)", title="Volkswagen")  +
    theme(aspect.ratio = 0.9)
  myplot_volks
  
  # Combine the 4 figures.
  library(ggpubr)
  Four_manufacturers = ggarrange(myplot_dodge, myplot_ford, myplot_toyota, myplot_volks,
                                 ncol=2, nrow=2)
  Four_manufacturers
  
  # It may look crowded on the plot preview window, but you can enlarge it to view it better.
  
  # Add a title to the combined figure.
  annotate_figure(Four_manufacturers, top=text_grob("Four manufacturers compared", 
                                                    face="bold", size = 15) )
# ===========================================================================

# ===========================================================================  
 # Make colors less vivid and easier to the eyes.
  library(scales)
  data(mpg)
  mpg$year_f = factor(mpg$year)

  mpg %>%
    filter( !is.na(year_f) ) %>%  # Pick up only those that are not NA as X axis.
    ggplot( aes(x=year_f, y=cty, fill=year_f ) ) +
    theme_bw(base_size = 15) +
    geom_boxplot(outlier.shape = NA) +  # Hide outliers if using jitter to avoid duplicated points.
    geom_jitter(color="black", width=0.2) +
    labs(x=element_blank(), y="City (mpg)" ) +
    theme(axis.text.x = element_blank()) +
    scale_fill_manual( values = c(muted("cyan"), muted("magenta")) ) 
# ===========================================================================

# ===========================================================================
# Useful codes for data analysis ============================================
# ===========================================================================

# ===========================================================================
  # Load necessary packages and dataset (in ggplot2).
  library(ggplot2)
  library(dplyr)
  data(mpg) # built within ggplot2 
  
# ===========================================================================
 # Copy Excel data and make it into a dataframe in R.
  # Copy (Ctrl + V) one column of cells in Excel, then,
  MYDATAFRAME = read.table(file="clipboard", sep=",")
  MYDATAFRAME
  
  # Copy (Ctrl + V) more than one columns of cells in Excel, then,
  MYDATAFRAME = read.table(file="clipboard", sep="\t")
  MYDATAFRAME
# ===========================================================================
  
# ===========================================================================
 # Rename a column.
  # Change the name of column 4.
  # colnames(MYDATAFRAME)[4] <- "MYNEWCOLUMNNAME"
  
  data(airquality)
  str(airquality)
  colnames(airquality)[4] <- "Temperature"
  str(airquality)
# ===========================================================================
  
# ===========================================================================
# Visualize missing data in each variable.
  data(airquality) # Load 'airquality' dataset which has some missing data
  head(airquality, 10) # View the first 10 rows in airquality
  
  library(naniar) # Load naniar package to use vis_miss function
  vis_miss(airquality)
# ===========================================================================

# ===========================================================================
 # Filter IN or OUT NA values. 
 # MYDATAFRAME %>% filter(  is.na(COLUMN_NAME) )
 # MYDATAFRAME %>% filter( !is.na(COLUMN_NAME) )
 
  data(airquality)
  Ozone_missing = airquality %>% filter( is.na(Ozone) )
  nrow(Ozone_missing)
  # 37 rows have missing data in variable Ozone.R.
  
  Ozone_nonmissing = airquality %>% filter( !is.na(Ozone) )
  nrow(Ozone_nonmissing)
  # 116 rows have non-missing data in variable Ozone.R.
# ===========================================================================

# ===========================================================================
 # Replace NAs with zero in a dataframe.
 # MYDATAFRAME1 <- MYDATAFRAME[ is.na(MYDATAFRAME) ] <- 0
  
  data(airquality)
  head(airquality)
  airquality[ is.na(airquality) ] <- 0
  head(airquality)
  # Please note that your dataframe will be overwritten.
# ===========================================================================

# ===========================================================================
 # Sort rows by a variable.
 # MYDATAFRAME[ order(MYDATAFRAME$MYCOLUMN), ]
 # OR
 # with the dplyr package
 # MYDATAFRAME %>%  arrange(MYCOLUMN)

  data(airquality)
  airquality[ order(airquality$Ozone), ]
  # OR
  data(airquality)
  airquality %>% arrange(Ozone)  
# ===========================================================================
  
# ===========================================================================
# Specify the order of factors to be displayed/plotted, instead of the default alphabetical/numerical order.
 # MYDATAFRAME$MYVARIABLE <- factor(MYDATAFRAME$MYVARIABLE, 
 #   levels=c("Level 4", "Level 2", "Level 1", "Level 3" )) (whatever order you like)
  
 # Check the order of the factors (as well as frequency)
 #  table(MYDATAFRAME$MYVARIABLE)
  
 # Sort according to the new order: the last item will be on top on a scatter plot.
 #  MYDATAFRAME_s <- MYDATAFRAME[order(MYDATAFRAME$MYVARIABLE, decreasing = F), ]
  
  data(airquality)
  table(mpg$class) 
  # factors are in the alphabetical order.
  
  # Change the order of factor levels according to the vehicle size.
  mpg$class_f = factor(mpg$class, 
                       levels = c( "2seater", "subcompact", "compact",  
                                   "midsize", "suv", "minivan", "pickup") )
  table(mpg$class_f)
  
  # Sort according to the new order.
  mpg_s = mpg[ order(mpg$class_f, decreasing = F), ]
  tail(mpg_s)
# ===========================================================================
  
# ===========================================================================
 # Take random sample rows from a dataframe.
  # Take two random rows from MYDATAFRAME.
  # MYDATAFRAME[ sample(nrow(MYDATAFRAME), 2), ]
  
  data(mpg)
  mpg[ sample(nrow(mpg), 2), ]
# ===========================================================================
  
# ===========================================================================
# Sum each column or row of a matrix.
 # Make a 4x3 matrix with random numbers
  m1 <- matrix( C<-(1:12), nrow=4, ncol=3 )
  m1
  
 # Add each row
  apply(m1, 1, sum)
 
 # Add each column
  apply(m1, 2, sum)
# ===========================================================================
  
# ===========================================================================
# Sum each column or row of a dataframe.
  # Get column totals
  # CTotal = colSums( t(MYDATAFRAME) )
  
  # Add column totals
  # MYDATAFRAMEcc = cbind(MYDATAFRAME, CTotal)
  
  # Add row totals
  # MYDATAFRAMEccrr = rbind(MYDATAFRAMEcc, colSums(MYDATAFRAMEcc))
  
  # Change the last rowname
  # rownames(MYDATAFRAMEccrr)[length(rownames(MYDATAFRAMEccrr))] <- "RTotal"

  # MYDATAFRAMEccrr
 
  # Make a 4x3 dataframe with random numbers
  d1 <- cbind.data.frame( Var1=c(1:4), Var2=c(5:8), Var3=c(9:12) )
  d1
 
 # Get sums of columns for each row
  CTotal = colSums( t(d1) )
  CTotal
 
 # Add sums of columns to the dataframe
  d1cc = cbind(d1, CTotal) 
  d1cc
  
 # Add row totals
  d1ccrr = rbind(d1cc, colSums(d1cc))
  d1ccrr
  
 # Change the last rowname
  rownames(d1ccrr)[ length(rownames(d1ccrr)) ] <- "RTotal"
  d1ccrr
# ===========================================================================  
  
# ===========================================================================  
 # Take average of multiple columns.
  # MYDATAFRAME$mean <- rowMeans(MYDATAFRAME[, c('MYCOLUMN1', ' MYCOLUMN2 ')], na.rm=TRUE)
  # na.rm=TRUE option ignores NA and gives average with existing data.
  
  d1 <- cbind.data.frame( Var1=c(1:4), Var2=c(5:8), Var3=c(9:12) )
  d1$mean12 = rowMeans(d1[, c('Var1', 'Var2')] )
  d1
# ===========================================================================  

# ===========================================================================  
 # Convert column(s) of a dataframe to numeric.
  # Convert 2nd-4th columns to numeric.
  # MYDATAFRAME[, c(2:4)] = sapply( MYDATAFRAME[, c(2:4)], as.numeric ) 
  
  data(mpg)
  str(mpg)
  # Convert the 4th-5th columns (year and cyl) in mpg as numeric
  mpg[ , c(4:5)] = sapply(mpg[, c(4:5)], as.numeric)
  str(mpg)
  # year and cyl are now numeric.
# ===========================================================================  
  
# ===========================================================================  
 # Apply a function to all the columns or rows. 

  # Calculate variation for each row. MARGIN=1 means apply to rows.
  # apply(MYDATAFRAME, MARGIN=1, var)
  
  # Calculate variation for each column. MARGIN=2 means apply to columns.
  # apply(MYDATAFRAME, MARGIN=2, var)
  
  # Prepare a practice dataframe called 'eg'.
  eg = cbind.data.frame( c(1,2,3,4,5,6), c(2,2,2,2,2,2) )
  colnames(eg) = c("AAA", "BBB")
  eg
  
  # Calculate variation for each row of eg. MARGIN=1 means apply to rows.
  apply(eg, MARGIN=1, var)
  
  # Calculate standard deviation for each column of eg. MARGIN=2 means apply to columns.
  apply(eg, MARGIN=2, sd) 
# ===========================================================================    

# ===========================================================================    
# Replace character strings in a dataframe. 
  # Replace 'Original text' with 'Replaced text'.
  # also applies to a part of text. 
  
  # MYDATAFRAME$MYVARIABLE <- gsub('Original text',
  #                                'Replaced text',
  #                                MYDATAFRAME$MYVARIABLE) 
  
  data(mpg)
  head(mpg)
  # Replace "compact" in the class variable with "COMPACT"
  mpg$class <- gsub('compact', 'COMPACT', mpg$class)
  head(mpg)
  table(mpg$class)
  # Note that 'subcompact' is also replaced with 'subCOMPACT'.
# ===========================================================================    

