# BOXPLOTS ---------------------------------------------------------------------
# 1. Preparation ---------------------------------------------------------------

rm(list = ls())

# Read data table 
data <- read.csv2("data/Anova.csv",na="NA", dec=",")

# Load libraries need
library(ggplot2)
library(extrafont)
library(ggpubr)

# vector to remove irrellevant x-axis ticks
xticks <- c(11, 14, 17, 2, 5, 8)

# vector to label x-axis
sites <- c("KI", "CH", "VC","KA", "UB", "VR")
nosites <- c("", "", "", "", "", "")

# 2. Data visualization of biometric and health parameters ---------------------

# Epiphyte Load

load <- ggplot(data, aes(x=factor(Block), y= Epiphyte.Load, 
                          color = factor(Condition)))+
  geom_boxplot(aes(color = factor(Condition)), position = position_dodge(2))+
  geom_point(position=position_jitterdodge(), alpha = 0.4, size = 1)+        
  facet_wrap(~Condition, scales = "free_x")+                                 
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black"),    
        strip.background = element_rect(color="black", fill="white"),        
        plot.title = element_text(hjust=0.5),                                
        text=element_text(family="Helvetica", face="bold", size=12))+                                   
  scale_color_manual(values = c("CS" = "#59a89c", "PDS" = "#cf3759"))+      
  labs(y=expression(bold(paste("gg"^"-1", "DW"))))+
  xlab(NULL)+
  ggtitle("Epiphyte load")+
  scale_x_discrete(breaks = xticks,                                         
                   labels = sites)

load

# Leaf area index

lai <- ggplot(data, aes(x=factor(Block), y= LAI, 
                         color = factor(Condition)))+
  geom_boxplot(aes(color = factor(Condition)), position = position_dodge(2))+
  geom_point(position=position_jitterdodge(), alpha = 0.4, size = 1)+        
  facet_wrap(~Condition, scales = "free_x")+                                 
  theme(legend.position = "none",     
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black"),    
        strip.background = element_rect(color="black", fill="white"),        
        plot.title = element_text(hjust=0.5),                                
        text=element_text(family="Helvetica", face="bold", size=12))+       
  scale_x_discrete(breaks = xticks,                                          
                   labels = nosites)+                                          
  scale_color_manual(values = c("CS" = "#457983", "PDS" = "#cf3759"))+    
  ylab("LAI")+
  xlab(NULL)+
  ggtitle("Leaf area index")

lai

# Shoot denisty

shoots <- ggplot(data, aes(x=factor(Block), y= Shoot.Denisity, 
                           color = factor(Condition)))+
  geom_boxplot(aes(color = factor(Condition)), position = position_dodge(2))+
  geom_point(position=position_jitterdodge(), alpha = 0.4, size = 1)+        
  facet_wrap(~Condition, scales = "free_x")+                                 
  theme(legend.position = "none",                                            
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black"),    
        strip.background = element_rect(color="black", fill="white"),        
        plot.title = element_text(hjust=0.5),                                
        text=element_text(family="Helvetica", face="bold", size=12))+        
  scale_x_discrete(breaks = xticks,                                          
                   labels = nosites)+                                          
  scale_color_manual(values = c("CS" = "#457983", "PDS" = "#cf3759"))+       
  labs(y=expression(bold(paste("Shoots per m"^"2"))))+
  xlab(NULL)+
  ggtitle("Shoot density")

shoots

# Leaf length

length <- ggplot(data, aes(x=factor(Block), y= Leave.Lenght, 
                            color = factor(Condition)))+
  geom_boxplot(aes(color = factor(Condition)), position = position_dodge(2))+
  geom_point(position=position_jitterdodge(), alpha = 0.4, size = 1)+        
  facet_wrap(~Condition, scales = "free_x")+                                 
  theme(legend.position = "none",                                            
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black"),    
        strip.background = element_rect(color="black", fill="white"),        
        plot.title = element_text(hjust=0.5),                                
        text=element_text(family="Helvetica", face="bold", size=12))+        
  scale_x_discrete(breaks = xticks,                                          
                   labels = nosites)+                                          
  scale_color_manual(values = c("CS" = "#457983", "PDS" = "#cf3759"))+       
  ylab("Length (cm)")+
  xlab(NULL)+
  ggtitle("Leaf length")+
  theme(plot.background = element_blank())

length

# Leaf width

width <- ggplot(data, aes(x=factor(Block), y= Leave.Width, 
                           color = factor(Condition)))+
  geom_boxplot(aes(color = factor(Condition)), position = position_dodge(2))+
  geom_point(position=position_jitterdodge(), alpha = 0.4, size = 1)+        
  facet_wrap(~Condition, scales = "free_x")+                                 
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black"),    
        strip.background = element_rect(color="black", fill="white"),        
        plot.title = element_text(hjust=0.5),                                
        text=element_text(family="Helvetica", face="bold", size=12))+        
  scale_x_discrete(breaks = xticks,                                          
                   labels = nosites)+                                          
  scale_color_manual(values = c("CS" = "#457983", "PDS" = "#cf3759"))+       
  ylab("Width (cm)")+
  xlab(NULL)+
  ggtitle("Leaf width")+
  theme(plot.background = element_blank())

width

# Total coverage

total <- ggplot(data, aes(x=factor(Block), y= Total.Coverage, 
                           color = factor(Condition)))+
  geom_boxplot(aes(color = factor(Condition)), position = position_dodge(2))+
  geom_point(position=position_jitterdodge(), alpha = 0.4, size = 1)+        
  facet_wrap(~Condition, scales = "free_x")+                                 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black"),    
        strip.background = element_rect(color="black", fill="white"),        
        plot.title = element_text(hjust=0.5),                               
        text=element_text(family="Helvetica", face="bold", size=12))+        
  scale_x_discrete(breaks = xticks,                                          
                   labels = sites)+                                          
  scale_color_manual(values = c("CS" = "#457983", "PDS" = "#cf3759"))+       
  ylab("Percentage\n cover (%)")+
  xlab(NULL)+
  ggtitle("Total epiphyte cover")+
  theme(plot.background = element_blank())

total

# Plot all together
final_plot_health <- ggarrange(lai, width, length, shoots, load, total,
                               widths = c(2,2), nrow = 3, 
                               ncol = 2, align="v")
annotate_figure(final_plot_health, 
                  bottom = text_grob("Sites", face = "bold", size = 14))

final_plot_health

ggsave(filename = "Plots/Health_parameters.jpg", plot = final_plot_health,
       width = 14, height = 11, units = "cm")

# 3. Data visualization of epiphyte functional groups --------------------------

# Encrusting algae

ealgae <- ggplot(data, aes(x=factor(Block), y=ENCRUSTING.ALGAE, 
                           color = factor(Condition)))+
  geom_boxplot(aes(color = factor(Condition)), position = position_dodge(2))+
  geom_point(position=position_jitterdodge(), alpha = 0.4, size = 1)+        
  facet_wrap(~Condition, scales = "free_x")+                                
  theme(legend.position = "none",                                         
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black"),   
        strip.background = element_rect(color="black", fill="white"),      
        plot.title = element_text(hjust=0.5),                              
        text=element_text(family="Helvetica", face="bold", size=12))+       
  scale_x_discrete(breaks = xticks,                                         
                   labels = nosites)+                                         
  scale_color_manual(values = c("CS" = "#457983", "PDS" = "#cf3759"))+      
  ylab(NULL)+
  xlab(NULL)+
  ggtitle("Encrusting algae")

ealgae

# Filamentous algae

falgae <- ggplot(data, aes(x=factor(Block), y= FILAMENTOUS.ALGAE, 
                        color = factor(Condition)))+
  geom_boxplot(aes(color = factor(Condition)), position = position_dodge(2))+
  geom_point(position=position_jitterdodge(), alpha = 0.4, size = 1)+        
  facet_wrap(~Condition, scales = "free_x")+                                 
  theme(legend.position = "none",     
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black"),    
        strip.background = element_rect(color="black", fill="white"),        
        plot.title = element_text(hjust=0.5),                                
        text=element_text(family="Helvetica", face="bold", size=12))+        
  scale_x_discrete(breaks = xticks,                                          
                   labels = nosites)+                                          
  scale_color_manual(values = c("CS" = "#457983", "PDS" = "#cf3759"))+      
  ylab(NULL)+
  xlab(NULL)+
  ggtitle("Filamentous algae")

falgae

# Bryozoa

bryo <- ggplot(data, aes(x=factor(Block), y= BRYOZOA, 
                           color = factor(Condition)))+
  geom_boxplot(aes(color = factor(Condition)), position = position_dodge(2))+
  geom_point(position=position_jitterdodge(), alpha = 0.4, size = 1)+        
  facet_wrap(~Condition, scales = "free_x")+                                 
  theme(legend.position = "none",                                           
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black"),  
        strip.background = element_rect(color="black", fill="white"),   
        plot.title = element_text(hjust=0.5),                             
        text=element_text(family="Helvetica", face="bold", size=12))+       
  scale_x_discrete(breaks = xticks,                                        
                   labels = nosites)+                                         
  scale_color_manual(values = c("CS" = "#457983", "PDS" = "#cf3759"))+      
  ylab(NULL)+
  xlab(NULL)+
  ggtitle("Bryozoa")

bryo

# Hydrozoa

hydro <- ggplot(data, aes(x=factor(Block), y= HYDROZOA, 
                           color = factor(Condition)))+
  geom_boxplot(aes(color = factor(Condition)), position = position_dodge(2))+
  geom_point(position=position_jitterdodge(), alpha = 0.4, size = 1)+        
  facet_wrap(~Condition, scales = "free_x")+                               
  theme(legend.position = "none",                                       
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black"),  
        strip.background = element_rect(color="black", fill="white"),   
        plot.title = element_text(hjust=0.5),                            
        text=element_text(family="Helvetica", face="bold", size=12))+     
  scale_x_discrete(breaks = xticks,                                   
                   labels = nosites)+                                     
  scale_color_manual(values = c("CS" = "#457983", "PDS" = "#cf3759"))+     
  ylab(NULL)+
  xlab(NULL)+
  ggtitle("Hydrozoa")+
  theme(plot.background = element_blank())

hydro

# Annelida

anne <- ggplot(data, aes(x=factor(Block), y= ANNELIDA, 
                          color = factor(Condition)))+
  geom_boxplot(aes(color = factor(Condition)), position = position_dodge(2))+
  geom_point(position=position_jitterdodge(), alpha = 0.4, size = 1)+      
  facet_wrap(~Condition, scales = "free_x")+                               
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black"),  
        strip.background = element_rect(color="black", fill="white"),      
        plot.title = element_text(hjust=0.5),                              
        text=element_text(family="Helvetica", face="bold", size=12))+      
  scale_x_discrete(breaks = xticks,                                      
                   labels = sites)+                                       
  scale_color_manual(values = c("CS" = "#457983", "PDS" = "#cf3759"))+     
  ylab(NULL)+
  xlab(NULL)+
  ggtitle("Annelida")+
  theme(plot.background = element_blank())

anne

# Foraminifera

fora <- ggplot(data, aes(x=factor(Block), y= FORAMINIFERA, 
                          color = factor(Condition)))+
  geom_boxplot(aes(color = factor(Condition)), position = position_dodge(2))+
  geom_point(position=position_jitterdodge(), alpha = 0.4, size = 1)+        
  facet_wrap(~Condition, scales = "free_x")+                           
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black"),   
        strip.background = element_rect(color="black", fill="white"),    
        plot.title = element_text(hjust=0.5),                           
        text=element_text(family="Helvetica", face="bold", size=12))+    
  scale_x_discrete(breaks = xticks,                                      
                   labels = sites)+                                       
  scale_color_manual(values = c("CS" = "#457983", "PDS" = "#cf3759"))+    
  ylab(NULL)+
  xlab(NULL)+
  ggtitle("Foraminifera")+
  theme(plot.background = element_blank())

fora

# Plot all together
final_plot_fgroups <- ggarrange(ealgae, falgae, bryo, hydro, anne, fora,
                               widths = c(2,2), nrow = 3, 
                               ncol = 2, align="v") %>% 
  annotate_figure(final_plot_health, 
                  bottom = text_grob("Sites", face = "bold", size = 14),
                  left = text_grob("Percentage cover (%)",
                                   face = "bold", size = 14, rot = 90))

final_plot_fgroups

ggsave(filename = "Plots/Fgroups.jpg", plot = final_plot_fgroups,
       width = 25, height = 20, units = "cm")


# MULTIVARIATE ANALYSIS --------------------------------------------------------

# 1. Preparation ---------------------------------------------------------------

# Remove all objects
rm(list = ls(all=TRUE))

# packages needed 
library(psych)
library(MASS)
library(vegan)
library(ggvegan)
library(ggplot2)
library(tidyverse)
library(BiodiversityR)   
library(dplyr)
library(ggrepel)

# Import species data
epi <- read.csv2("data/Species_data.csv",na="NA", dec=",")

# Import environmental data
env <- read.csv2("data/Environmental_data.csv",na="NA", dec=",")

# Rename columns --> Remove dots in names
names(env) <- c("Site", "Block No", "Sample ID", "Site Code", "Meadow Coverage",
                "Epiphyte Load", "LAI", "LUSI", "Sediment type", "Turbidity", 
                "Light intensity", "Temperature")
# check
names(env)

# check scale level of variables 
str(epi)
str(env)

# 2. Run nMDS ------------------------------------------------------------------

# log(x+1)-transformation 
epi <- log(epi +1)

# set random number generator for similar results each run
set.seed(991992)

# perform nMDS
nmds1 <- metaMDS(epi, distance = "bray", k = 3, 
                 autotransform = FALSE, trymax = 20)

# Plot nMDS
plot(nmds1)

# Results of different attempts with varying dimensions:

# dimensions  stress value    runs
#   6              /          27    # no convergence
#   5              /          24    # no convergence
#   4              0.09       20    # similar to 3 dimensions
#   3              0.11       20    # similar to 4 dimensions # best solution
#   2              0.15       20    # stress too high

# Report stress value
nmds1$stress

# 3. Run fit of environmental variables ----------------------------------------

# Calculating fit of environmental data / create environmental vectors

# Only site column needed, remove Block and Sample ID column
# delete column "Block"
env <- env[, -2]
env

# delete column "Sample_ID"
env <- env[, -2]

# check data frame
str(env)

# fit environmental vector onto ordination
fit_env <- envfit(nmds1, env, perm = 1000)
fit_env

# plot only significant environmental vectors
ordiplot(nmds1,type="t")
plot(fit_env, p.max = 0.05)

# 4. Extract NMDS-values and environmental vectors -----------------------------

# extract NMDS coordinates to a new data frame
site.scrs <- as.data.frame(scores(nmds1, display = "site"))
site.scrs
plot(site.scrs)

# add site column from env data-frame to the NMDS scores
site.scrs <- cbind(site.scrs, Sites = env$Site)
site.scrs

# add data frame with the condition value from the "all environmental data" frame
site.scrs <- cbind(site.scrs, Condition = env$`Site Code`)
site.scrs

# extracts environmental variables (vectors) from fit_env
env.scores <- as.data.frame(scores(fit_env, display = "vectors"))
env.scores

# adds their names to the data frame
env.scores <- cbind(env.scores,  Env.variables = rownames(env.scores)) 
env.scores

# adds their p-value to the data frame 
env.scores <- cbind(env.scores, p.value = fit_env$vectors$pvals) 
env.scores

# create new subset data with only significant environmental variables at .05
sig.env.scrs <- subset(env.scores, p.value<=0.05) 
sig.env.scrs

# 6. Create final nMDS plot ----------------------------------------------------

# Preparation of axis and frame (only x-axis and y-axis, no frame)

# manual axis from -1 to 1
axis_begin  <- -1
axis_end    <- 1

# total of 10 ticks
total_ticks <- 10

# create the tick frame
tick_frame <- data.frame(
  ticks = seq(axis_begin, axis_end, length.out = total_ticks), zero=0) %>%
  subset(ticks != 0)

# create the lab frame
lab_frame <- data.frame(lab = seq(axis_begin, axis_end), zero = 0) %>%
  subset(lab != 0)

# bind them together
tick_sz <- (tail(lab_frame$lab, 1) -  lab_frame$lab[1]) / 128

# final plot
final_NMDS <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2)) + 
  geom_segment(x = 0, xend = 0, y = lab_frame$lab[1],
               yend = tail(lab_frame$lab, 1), size = 0.5) +
  geom_segment(y = 0, yend = 0, x = lab_frame$lab[1], 
               xend = tail(lab_frame$lab, 1), size = 0.5) +
  geom_segment(data = tick_frame, aes(x = ticks, xend = ticks, 
                   y = zero, yend = zero + tick_sz)) +
  geom_segment(data = tick_frame, aes(x = zero, xend = zero + tick_sz, 
                   y = ticks, yend = ticks)) + 
  geom_point(size = 3, color = "white",
             aes(NMDS1, NMDS2, shape = factor(Sites), fill = Condition))+
  scale_color_manual(values=c("#59a89c", "#cf3759"))+ 
  scale_shape_manual(values=c(21, 21, 24, 22, 22, 24))+ 
  scale_fill_manual(values=c("#59a89c", "#cf3759"))+
  theme_void() +
  geom_segment(data = sig.env.scrs, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), 
               arrow = arrow(length = unit(0.1, "cm")), 
               colour = "black", lwd=0.5) + 
  ggrepel::geom_text_repel(data = sig.env.scrs, 
                           aes(x=NMDS1*1.5, y=NMDS2*1.7, label = Env.variables), 
                           cex = 3.5, direction = "both", segment.size = 0.4)+ 
  theme(aspect.ratio = 1)+
  annotate(geom="text", x=0.9, y=-0.8, label="Stress: 0.096", cex = 4.5)+
  theme(legend.position='none')

final_NMDS

# save and export
ggsave(filename = "plots/NMDS.jpg", plot = final_NMDS,
       width = 14, height = 14, units = "cm")



# UNIVARIATE ANALYSIS ----------------------------------------------------------

# 1. Preparation ---------------------------------------------------------------

# Remove all objects
rm(list = ls(all=TRUE))

# import data
data <- read.csv2("data/anova.csv")

# factorize variables 
data$Site <- as.factor(data$Site)
data$Block <- as.factor(data$Block)
data$Condition <- as.factor(data$Condition)

# 2. Health parameters ---------------------------------------------------------
# leaf area index --------------------------------------------------------------

# run model
a.lai <- aov(LAI~Condition + Error(Site/Block), data = data)
summary(a.lai)

# check model assumptions
plot(lm(a.lai), which = 2)
hist(residuals(a.lai$Within))
plot(lm(a.lai), which = 1) # patterns visible

# Log-Transformation
data$log_lai <- log(data$LAI)

# nested ANOVA
a.lai_log <- aov(log_lai~Condition + Error(Site/Block), data = data)
summary(a.lai_log)

# check model assumptions
plot(lm(a.lai_log), which = 2)
hist(residuals(a.lai_log$Within))
plot(lm(a.lai_log), which = 1) # assumptions given


# Leaf length ------------------------------------------------------------------
# run model
a.leaf.len <- aov(Leave.Lenght~Condition + Error(Site/Block), data = data)
summary(a.leaf.len)

# check model assumptions
plot(lm(a.leaf.len), which = 2)
plot(lm(a.leaf.len), which = 1) # patterns visible 

# Log-Transformation
data$log_leaf.len <- log(data$Leave.Lenght + 1)

# nested ANOVA
a.leaf.len_log <- aov(log_leaf.len~Condition + Error(Site/Block), data = data)
summary(a.leaf.len_log)

# check model assumptions
plot(lm(a.leaf.len_log), which = 2)
hist(residuals(a.leaf.len_log$Within))
plot(lm(a.leaf.len_log), which = 1) # assumptions given

# Shoot density ----------------------------------------------------------------

# run model
a.shoot <- aov(Shoot.Denisity~Condition + Error(Site/Block), data = data)
summary(a.shoot)

# check model assumptions
plot(lm(a.shoot), which = 2)
hist(residuals(a.shoot$Within))
plot(lm(a.shoot), which = 1) # given

# Leaf width -------------------------------------------------------------------

# run model
a.lea.wid <- aov(Leave.Width~Condition + Error(Site/Block), data = data)
summary(a.lea.wid)

# check model assumptions
plot(lm(a.lea.wid), which = 2)
hist(residuals(a.lea.wid$Within))
plot(lm(a.lea.wid), which = 1) # given 

# Total coverage ---------------------------------------------------------------

# run model
a.total <- aov(Total.Coverage~Condition + Error(Site/Block), data = data)
summary(a.total)

# check model assumptions
plot(lm(a.total), which = 2)
plot(lm(a.total), which = 1) # strong patterns  

# Log-Transformation
data$log_total <- log(data$Total.Coverage)

# nested ANOVA
a.total_log <- aov(log_total~Condition + Error(Site/Block), data = data)
summary(a.total_log)

# check model assumptions
plot(lm(a.total_log), which = 2)
hist(residuals(a.total_log$Within))
plot(lm(a.total_log), which = 1) # no patterns visible 

# Epiphyte load ----------------------------------------------------------------

# run model
a.epi <- aov(Epiphyte.Load~Condition + Error(Site/Block), data = data)
summary(a.epi)

# check model assumptions
plot(lm(a.epi), which = 2) # not given 
hist(residuals(a.epi$Within)) # not given
plot(lm(a.epi), which = 1) # patterns visible 

# Log-Transformation
data$log_epi_load <- log(data$Epiphyte.Load + 1)

# nested ANOVA
a.epi_log <- aov(log_epi_load~Condition + Error(Site/Block), data = data)
summary(a.epi_log)

# check model assumptions
plot(lm(a.epi_log), which = 2)
hist(residuals(a.epi_log$Within))
plot(lm(a.epi_log), which = 1) # assumption given 

# Sqrt-Transformation
data$sqrt_epi_load <- sqrt(data$Epiphyte.Load)

# nested ANOVA
a.epi_sqrt <- aov(sqrt_epi_load~Condition + Error(Site/Block), data = data)
summary(a.epi_sqrt)

# check model assumptions
plot(lm(a.epi_sqrt), which = 2)
hist(residuals(a.epi_sqrt$Within))
plot(lm(a.epi_sqrt), which = 1) # less patterns

# 3. Functional groups ---------------------------------------------------------
# Filamentous algae ------------------------------------------------------------

# run model
a.fil_algae <- aov(FILAMENTOUS.ALGAE~Condition + Error(Site/Block), data = data)
summary(a.fil_algae)

# check model assumptions
plot(lm(a.fil_algae), which = 2) # not linear
hist(residuals(a.fil_algae$Within))
plot(lm(a.fil_algae), which = 1) # strong patterns

# Log-Transformation
data$log_fil_algae <- log(data$FILAMENTOUS.ALGAE +1)

# nested ANOVA
a.fil_algae_log <- aov(log_fil_algae~Condition + Error(Site/Block), data = data)
summary(a.fil_algae_log)

# check model assumptions
plot(lm(a.fil_algae_log), which = 2)
hist(residuals(a.fil_algae_log$Within))
plot(lm(a.fil_algae_log), which = 1) # assumptions given



# Encrusting algae -------------------------------------------------------------

# run model
a.enc_algae <- aov(ENCRUSTING.ALGAE~Condition + Error(Site/Block), data = data)
summary(a.enc_algae)

# check model assumptions
plot(lm(a.enc_algae), which = 2) 
plot(lm(a.enc_algae), which = 1) # strong pattern

# Log-Transformation
data$log_enc_algae <- log(data$ENCRUSTING.ALGAE + 1)

# nested ANOVA: log_enc_algae
a.enc_algae_log <- aov(log_enc_algae~Condition + Error(Site/Block), data = data)
summary(a.enc_algae_log)

# check model assumptions
plot(lm(a.enc_algae_log), which = 2)
hist(residuals(a.enc_algae_log$Within))
plot(lm(a.enc_algae_log), which = 1) # assumptions given

# Bryozoans --------------------------------------------------------------------

# run model
a.bryozoa <- aov(BRYOZOA~Condition + Error(Site/Block), data = data)
summary(a.bryozoa)

# check model assumptions
plot(lm(a.bryozoa), which = 2) 
hist(residuals(a.bryozoa$Within))
plot(lm(a.bryozoa), which = 1) # strong patterns visible

# Log-Transformation
data$log_bryo <- log(data$BRYOZOA +1 )

# attach log_algae to data frame
data <- mutate(data, log_bryo)

# nested ANOVA: log_bryo
a.bryo_log<- aov(log_bryo~Condition + Error(Site/Block), data = data)
summary(a.bryo_log)

# check model assumptions
plot(lm(a.bryo_log), which = 2)
hist(residuals(a.bryo_log$Within))
plot(lm(a.bryo_log), which = 1) # better

# Foraminifera -----------------------------------------------------------------

# run model
a.fora <- aov(FORAMINIFERA~Condition + Error(Site/Block), data = data)
summary(a.fora)

# check model assumptions
plot(lm(a.fora), which = 2) 
hist(residuals(a.fora $Within))
plot(lm(a.fora), which = 1) # patterns visible, transformation needed

# Log-Transformation
data$log_fora <- log(data$FORAMINIFERA + 1)

# attach log_fora to data frame
data <- mutate(data, log_fora)

# nested ANOVA
a.fora_log<- aov(data$log_fora~Condition + Error(Site/Block), data = data)
summary(a.fora_log)

# check model assumptions
plot(lm(a.fora_log), which = 2)
hist(residuals(a.fora_log$Within))
plot(lm(a.fora_log), which = 1) # still patterns

# Sqrt-Transformation
data$sqrt_fora <- sqrt(data$FORAMINIFERA)

# attach sqrt_fora to data frame
data <- mutate(data, sqrt_fora)

# nested ANOVA
a.fora_sqrt<- aov(sqrt_fora~Condition + Error(Site/Block), data = data)
summary(a.fora_sqrt)

# check model assumptions
plot(lm(a.fora_sqrt), which = 2)
hist(residuals(a.fora_sqrt$Within))
plot(lm(a.fora_sqrt), which = 1) # assumption given 

# Annelida ---------------------------------------------------------------------

# run model
a.anne <- aov(ANNELIDA~Condition + Error(Site/Block), data = data)
summary(a.anne)

# check model assumptions
plot(lm(a.anne), which = 2)
plot(lm(a.anne), which = 1) # patterns visible, transformation needed 

# Log-Transformation
data$log_anne <- log(data$ANNELIDA +1)

# attach log_anne to data frame
data <- mutate(data, log_anne)

# nested ANOVA
a.anne_log <- aov(log_anne~Condition + Error(Site/Block), data = data)
summary(a.anne_log)

# check model assumptions
plot(lm(a.anne_log), which = 2)
hist(residuals(a.anne_log$Within))
plot(lm(a.anne_log), which = 1) # no patterns visible 

# Sqrt-Transformation
data$sqrt_anne <- sqrt(data$ANNELIDA)

# attach sqrt_algae to data frame
data <- mutate(data, sqrt_anne)

# nested ANOVA: sqrt_anne
a.anne_sqrt <- aov(sqrt_anne~Condition + Error(Site/Block), data = data)
summary(a.anne_sqrt)

# check model assumptions
plot(lm(a.anne_sqrt), which = 2)
hist(residuals(a.anne_sqrt$Within))
plot(lm(a.anne_sqrt), which = 1) # given

# Hydrozoa ---------------------------------------------------------------------

# run model
a.hydro <- aov(HYDROZOA~Condition + Error(Site/Block), data = data)
summary(a.hydro)

# check model assumptions
plot(lm(a.hydro), which = 2)
hist(residuals(a.hydro$Within))
plot(lm(a.hydro), which = 1)

# Log-Transformation
data$log_hydro <- log(data$HYDROZOA + 1)

# attach log_hydro to data frame
data <- mutate(data, log_hydro)

# nested ANOVA: log_hydro
a.hydro_log <- aov(log_hydro~Condition + Error(Site/Block), data = data)
summary(a.hydro_log)

# check model assumptions
plot(lm(a.hydro_log), which = 2)
hist(residuals(a.hydro_log$Within))
plot(lm(a.hydro_log), which = 1) # patterns visible 

# Sqrt-Transformation
data$sqrt_hydro <- sqrt(data$HYDROZOA)

# attach sqrt_algae to data frame
data <- mutate(data, sqrt_hydro)

# nested ANOVA: sqrt_hydro
a.hydro_sqrt <- aov(sqrt_hydro~Condition + Error(Site/Block), data = data)
summary(a.hydro_sqrt)

# check model assumptions
plot(lm(a.hydro_sqrt), which = 2)
hist(residuals(a.hydro_sqrt$Within))
plot(lm(a.hydro_sqrt), which = 1) # given

