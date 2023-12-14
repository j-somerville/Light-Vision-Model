##Model script for generating values for species' photoreceptor stimulation
##to different coloured LEDs in different water conditions
##Quantum-catch model

#set WD

############Data files####################
##LED COLOURS - irradiance measured 50cm from LED in dark room 
###7 different colour modes.
LED<-read.csv("LED_normalised_new.csv")
##species - generated from inputting peak sensitivities into Pigment Model excel doc.
##which is based on Govardovskiĭ et al 2000 pigment model
species<-read.csv("bycatch_target_sensitivity.csv")
##ocular media files - based on Thorpe et al 1993 pigment classification
###or extracted from the literature from measurements (see Species vision document)
ray_lens<-read.csv("raja_lens_processed.csv")
cod_lens<-read.csv("gadoid_lens_processed.csv")
##need to add 1 up to 700nm
juvenile_yellowfin_lens<-read.csv("juvenile_yellowfin_lens_processed.csv")
sole_lens<-read.csv("raja_lens_processed.csv")
salmon_lens<-read.csv("raja_lens_processed.csv")
##same T50 value as ray
walleye_lens<-read.csv("gadoid_lens_processed.csv")
##gadoid family
juvenile_hammerhead_lens<-read.csv("hammerhead_lens_processed.csv")
##check all 401 in length

##water type backgrounds - extracted from Sticklus 2015, based on Jerlov's ocean quantification
jerlovIb<-read.csv("jerlovIb.csv")
jerlovIII<-read.csv("jerlovIII.csv")

library(dplyr)

##-LED data is normalised for each colour mode (see 'normalising LED spectra' r script)
##species relative sensitivity is the photoreceptor curve for each species (derived from pigment template Govardovskii et al 2000)
##-All relative sensitivities are normalised between 0 and 1.
##-Categories = photoreceptor types (shorter wavelength: sw, medium: mw, longer: lw, rod)
##species = cod, nephrops
##dl=dark,light, unknown (the ambient light conditions that photoreceptor sensitivity was measured in, see Species vision)
##-Aim is to multiply each LED colour mode spectra by each species' sensitivity, 
##then get overall sum of photoreceptor stimulation for different light colour modes
##and for each photoreceptor a species has
##Do the same for background  - multiply photoreceptor sensitivity by background
##then get a ratio of LED to background as final model output
##-Ocular media files (how light transmits through eye structures before retina) for different species
##-These files have been processed from graph extraction software (see 'Ocular media extraction instructions' r script)


##########Species dataframe sorting####################
species_split <- split(species, paste0(species$photoreceptor,sep="_", species$species, sep="_", species$dl))
##split by species, photoreceptor type and dl
##when using bigger dataset, should be maximum of 4 different photoreceptor type
##so maximum of 4 lists per species.
species_split[["lw_salmon_dark"]]
##check out a particular list

a<-lapply(species_split, `[`, 4)

##create a dataframe that reduces lists to just the sensitivity of species,removes every column apart from 4th
#use 'a' to get seperate columns for each species
library("tidyverse")
species_rename<-map2(a, names(a),~rename_all(.x, function(z) paste(.y, z, sep = ".")))
species_df<-(as.data.frame(species_rename))
##add wavelength values in 1nm increments
species_df$wavelength=(300:700)

##next step is to multiply ocular media by photoreceptor sensitivity
##as each ocular media sensitivity is a different file for each species,
##there is separate lines of code for this, per species

#thornbackray
species_df$rod_thornbackray_unknown_OM_sensitivity<-species_df$rod_thornback.ray_unknown.sensitivity*ray_lens$sensitivity
##cod sw
species_df$sw_cod_light_OM_sensitivity<-species_df$sw_cod_light.sensitivity*cod_lens$sensitivity
##cod mw
species_df$mw_cod_light_OM_sensitivity<-species_df$mw_cod_light.sensitivity*cod_lens$sensitivity
##yellowfin rod
species_df$rod_yellowfintuna_dark_OM_sensitivity<-species_df$rod_yellowfin.tuna_dark.sensitivity*juvenile_yellowfin_lens$sensitivity
##yellow fin tuna sw
species_df$sw_yellowfintuna_dark_OM_sensitivity<-species_df$sw_yellowfin.tuna_dark.sensitivity*juvenile_yellowfin_lens$sensitivity
##yellow fin tuna mw
species_df$mw_yellowfintuna_dark_OM_sensitivity<-species_df$mw_yellowfin.tuna_dark.sensitivity*juvenile_yellowfin_lens$sensitivity
##sole sw
species_df$sw_sole_dark_OM_sensitivity<-species_df$sw_sole_dark.sensitivity*sole_lens$sensitivity
#sole mw1
species_df$mw1_sole_dark_OM_sensitivity<-species_df$mw1_sole_dark.sensitivity*sole_lens$sensitivity
#sole mw2
species_df$mw2_sole_dark_OM_sensitivity<-species_df$mw2_sole_dark.sensitivity*sole_lens$sensitivity
#sole lw
species_df$lw_sole_dark_OM_sensitivity<-species_df$lw_sole_dark.sensitivity*sole_lens$sensitivity
#sole rod
species_df$rod_sole_dark_OM_sensitivity<-species_df$rod_sole_dark.sensitivity*sole_lens$sensitivity
#salmon sw
species_df$sw_salmon_dark_OM_sensitivity<-species_df$sw_salmon_dark.sensitivity*salmon_lens$sensitivity
#salmon mw
species_df$mw_salmon_dark_OM_sensitivity<-species_df$mw_salmon_dark.sensitivity*salmon_lens$sensitivity
#salmon lw
species_df$lw_salmon_dark_OM_sensitivity<-species_df$lw_salmon_dark.sensitivity*salmon_lens$sensitivity
#salmon rod
species_df$rod_salmon_dark_OM_sensitivity<-species_df$rod_salmon_dark.sensitivity*salmon_lens$sensitivity
#walleye mw
species_df$mw_walleyepollock_dark_OM_sensitivity<-species_df$mw_walleyepollock_dark.sensitivity*walleye_lens$sensitivity
##hammerhead rod
species_df$rod_juvenilehammerhead_dark_OM_sensitivity<-species_df$rod_juvenilehammerhead_dark.sensitivity*juvenile_hammerhead_lens$sensitivity


##Create new dataframe for ocular media values
ocularmedia<-select(species_df, contains("OM_"))
##now to normalise all columns
##can apply a function to dataframe
normalise <- function(x, na.rm = TRUE) {
  ranx <- range(x, na.rm = na.rm)
  (x - ranx[1]) / diff(ranx)
}

ocularmedia1<-lapply(ocularmedia, FUN=normalise)
##makes into a list

#now to multiply every column in the list by LED values to get quantum catch
##values
white<-sapply(ocularmedia1,FUN= function(x) x*LED$white.norm)
colnames(white) <- paste("white", colnames(white), sep = "_")
##adds prefix of 'white' to all column names. repeat for all colour modes
blue<-sapply(ocularmedia1, FUN= function(x) x*LED$blue.norm)
colnames(blue) <- paste("blue", colnames(blue), sep = "_")
green<-sapply(ocularmedia1,FUN= function(x) x*LED$green.norm)
colnames(green) <- paste("green", colnames(green), sep = "_")
cyan<-sapply(ocularmedia1,FUN= function(x) x*LED$cyan.norm)
colnames(cyan) <- paste("cyan", colnames(cyan), sep = "_")
red<-sapply(ocularmedia1,FUN= function(x) x*LED$red.norm)
colnames(red) <- paste("red", colnames(red), sep = "_")
royalblue<-sapply(ocularmedia1,FUN= function(x) x*LED$royalblue.norm)
colnames(royalblue) <- paste("royalblue", colnames(royalblue), sep = "_")
amber<-sapply(ocularmedia1,FUN= function(x) x*LED$amber.norm)
colnames(amber) <- paste("amber", colnames(amber), sep = "_")

allcolours<-cbind(blue,white,cyan,green,red,royalblue,amber)
allcolours<-as.data.frame(allcolours)
#write.csv(allcolours, "allcoloursquantumcatch1nm.csv")

#########Quantum catch colour modes###########
##sum all colour modes to get quantum catch for each photoreceptor type of each species, for each colour mode
qcvalues<-colSums(allcolours)
qcvalues<-as.data.frame(qcvalues)
##adds new column with row name filled, so now dataframe has 2 columns
qcvalues<-setNames(cbind(rownames(qcvalues), qcvalues, row.names = NULL), 
                   c("species", "QC.light"))
##these are the 'raw' Quatum catch values, assuming no background light
##next step would be to find background irradiance total 
##sort original qc dataframe by colour
##separate by each "_"
library("stringr")
qcvalues2<-as.data.frame(str_split_fixed(qcvalues$species, "_", 6))
qcvalues3<-cbind(qcvalues2, qcvalues$QC.light)
##get rid of unnecessary OM and sensitivity column
qcvalues3<-subset(qcvalues3, select = -c(V5,V6))
colnames(qcvalues3)[1]=("colour")
colnames(qcvalues3)[2]=("photoreceptor")
colnames(qcvalues3)[3]=("species")
colnames(qcvalues3)[4]=("light_conditions_photoreceptors")
colnames(qcvalues3)[5]=("quantum_catch_OM")

###subset for each colour
amber<-subset(qcvalues3, qcvalues3$colour=="amber")
royalblue<-subset(qcvalues3, qcvalues3$colour=="royalblue")
blue<-subset(qcvalues3, qcvalues3$colour=="blue")
cyan<-subset(qcvalues3, qcvalues3$colour=="cyan")
green<-subset(qcvalues3, qcvalues3$colour=="green")
white<-subset(qcvalues3, qcvalues3$colour=="white")
red<-subset(qcvalues3, qcvalues3$colour=="red")



##########Water type quantum catch##############
##need to multiply ocular media table by irradiance of water type for every nm,
##so use ocularmedia1 list and convert to df
ocularmedia_df<-as.data.frame(ocularmedia1)
ocularmedia_df$wavelength=(300:700)
#write.csv(ocularmedia_df, "ocularmedia_df.csv")




###Jerlov type III - semi turbid at 20m

jerlovIII_20m<-mapply(ocularmedia1,FUN= function(x) x*jerlovIII$irradiance_20m)
jerlovIII_20m<-as.data.frame(jerlovIII_20m)
qc_jerlovIII_20m<-colSums(jerlovIII_20m)
qc_jerlovIII_20m<-as.data.frame(qc_jerlovIII_20m)
##add new column with row name filled, so now dataframe has 2 columns
qc_jerlovIII_20m<-setNames(cbind(rownames(qc_jerlovIII_20m), qc_jerlovIII_20m, row.names = NULL), 
                           c("species", "qc_jerlovIII_20m"))


##divide LED qc by quantum catch background
amber$quantumcatch_jerlovIII_20m=amber$quantum_catch_OM/qc_jerlovIII_20m$qc_jerlovIII_20m
royalblue$quantumcatch_jerlovIII_20m=royalblue$quantum_catch_OM/qc_jerlovIII_20m$qc_jerlovIII_20m
blue$quantumcatch_jerlovIII_20m<-blue$quantum_catch_OM/qc_jerlovIII_20m$qc_jerlovIII_20m
cyan$quantumcatch_jerlovIII_20m<-cyan$quantum_catch_OM/qc_jerlovIII_20m$qc_jerlovIII_20m
green$quantumcatch_jerlovIII_20m<-green$quantum_catch_OM/qc_jerlovIII_20m$qc_jerlovIII_20m
white$quantumcatch_jerlovIII_20m<-white$quantum_catch_OM/qc_jerlovIII_20m$qc_jerlovIII_20m
red$quantumcatch_jerlovIII_20m<-red$quantum_catch_OM/qc_jerlovIII_20m$qc_jerlovIII_20m
qc_final_jerlovIII<-rbind(royalblue,blue,cyan,green,amber,red,white)

##50m

jerlovIII_50m<-mapply(ocularmedia1,FUN= function(x) x*jerlovIII$irradiance_50m)
jerlovIII_50m<-as.data.frame(jerlovIII_50m)
qc_jerlovIII_50m<-colSums(jerlovIII_50m)
qc_jerlovIII_50m<-as.data.frame(qc_jerlovIII_50m)
##add new column with row name filled, so now dataframe has 2 columns
qc_jerlovIII_50m<-setNames(cbind(rownames(qc_jerlovIII_50m), qc_jerlovIII_50m, row.names = NULL), 
                           c("species", "qc_jerlovIII_50m"))


###Jerlov III 50m
amber$quantumcatch_jerlovIII_50m=amber$quantum_catch_OM/qc_jerlovIII_50m$qc_jerlovIII_50m
royalblue$quantumcatch_jerlovIII_50m=royalblue$quantum_catch_OM/qc_jerlovIII_50m$qc_jerlovIII_50m
blue$quantumcatch_jerlovIII_50m<-blue$quantum_catch_OM/qc_jerlovIII_50m$qc_jerlovIII_50m
cyan$quantumcatch_jerlovIII_50m<-cyan$quantum_catch_OM/qc_jerlovIII_50m$qc_jerlovIII_50m
green$quantumcatch_jerlovIII_50m<-green$quantum_catch_OM/qc_jerlovIII_50m$qc_jerlovIII_50m
white$quantumcatch_jerlovIII_50m<-white$quantum_catch_OM/qc_jerlovIII_50m$qc_jerlovIII_50m
red$quantumcatch_jerlovIII_50m<-red$quantum_catch_OM/qc_jerlovIII_50m$qc_jerlovIII_50m
qc_final_jerlovIII<-rbind(royalblue,blue,cyan,green,amber,red,white)

#Jerlov III 100m
jerlovIII_100m<-mapply(ocularmedia1,FUN= function(x) x*jerlovIII$irradiance_100m)
jerlovIII_100m<-as.data.frame(jerlovIII_100m)
qc_jerlovIII_100m<-colSums(jerlovIII_100m)
qc_jerlovIII_100m<-as.data.frame(qc_jerlovIII_100m)
##add new column with row name filled, so now dataframe has 2 columns
qc_jerlovIII_100m<-setNames(cbind(rownames(qc_jerlovIII_100m), qc_jerlovIII_100m, row.names = NULL), 
                            c("species", "qc_jerlovIII_100m"))


#100m
amber$quantumcatch_jerlovIII_100m=amber$quantum_catch_OM/qc_jerlovIII_100m$qc_jerlovIII_100m
royalblue$quantumcatch_jerlovIII_100m=royalblue$quantum_catch_OM/qc_jerlovIII_100m$qc_jerlovIII_100m
blue$quantumcatch_jerlovIII_100m<-blue$quantum_catch_OM/qc_jerlovIII_100m$qc_jerlovIII_100m
cyan$quantumcatch_jerlovIII_100m<-cyan$quantum_catch_OM/qc_jerlovIII_100m$qc_jerlovIII_100m
green$quantumcatch_jerlovIII_100m<-green$quantum_catch_OM/qc_jerlovIII_100m$qc_jerlovIII_100m
white$quantumcatch_jerlovIII_100m<-white$quantum_catch_OM/qc_jerlovIII_100m$qc_jerlovIII_100m
red$quantumcatch_jerlovIII_100m<-red$quantum_catch_OM/qc_jerlovIII_100m$qc_jerlovIII_100m
qc_final_jerlovIII<-rbind(royalblue,blue,cyan,green,amber,red,white)


#### jerlov IB (less turbid open ocean)
#20m
jerlovIb_20m<-mapply(ocularmedia1,FUN= function(x) x*jerlovIb$irradiance_20m)
jerlovIb_20m<-as.data.frame(jerlovIb_20m)
qc_jerlovIb_20m<-colSums(jerlovIb_20m)
qc_jerlovIb_20m<-as.data.frame(qc_jerlovIb_20m)
##add new column with row name filled, so now dataframe has 2 columns
qc_jerlovIb_20m<-setNames(cbind(rownames(qc_jerlovIb_20m), qc_jerlovIb_20m, row.names = NULL), 
                          c("species", "qc_jerlovIb_20m"))


##divide by quantum catch background!
amber$quantumcatch_jerlovIb_20m=amber$quantum_catch_OM/qc_jerlovIb_20m$qc_jerlovIb_20m
royalblue$quantumcatch_jerlovIb_20m=royalblue$quantum_catch_OM/qc_jerlovIb_20m$qc_jerlovIb_20m
blue$quantumcatch_jerlovIb_20m<-blue$quantum_catch_OM/qc_jerlovIb_20m$qc_jerlovIb_20m
cyan$quantumcatch_jerlovIb_20m<-cyan$quantum_catch_OM/qc_jerlovIb_20m$qc_jerlovIb_20m
green$quantumcatch_jerlovIb_20m<-green$quantum_catch_OM/qc_jerlovIb_20m$qc_jerlovIb_20m
white$quantumcatch_jerlovIb_20m<-white$quantum_catch_OM/qc_jerlovIb_20m$qc_jerlovIb_20m
red$quantumcatch_jerlovIb_20m<-red$quantum_catch_OM/qc_jerlovIb_20m$qc_jerlovIb_20m
qc_final_jerlovIb<-rbind(royalblue,blue,cyan,green,amber,red,white)


##jerlov ib 50m 

jerlovIb_50m<-mapply(ocularmedia1,FUN= function(x) x*jerlovIb$irradiance_50m)
jerlovIb_50m<-as.data.frame(jerlovIb_50m)
qc_jerlovIb_50m<-colSums(jerlovIb_50m)
qc_jerlovIb_50m<-as.data.frame(qc_jerlovIb_50m)
##add new column with row name filled, so now dataframe has 2 columns
qc_jerlovIb_50m<-setNames(cbind(rownames(qc_jerlovIb_50m), qc_jerlovIb_50m, row.names = NULL), 
                          c("species", "qc_jerlovIb_50m"))


##divide by quantum catch background!
amber$quantumcatch_jerlovIb_50m=amber$quantum_catch_OM/qc_jerlovIb_50m$qc_jerlovIb_50m
royalblue$quantumcatch_jerlovIb_50m=royalblue$quantum_catch_OM/qc_jerlovIb_50m$qc_jerlovIb_50m
blue$quantumcatch_jerlovIb_50m<-blue$quantum_catch_OM/qc_jerlovIb_50m$qc_jerlovIb_50m
cyan$quantumcatch_jerlovIb_50m<-cyan$quantum_catch_OM/qc_jerlovIb_50m$qc_jerlovIb_50m
green$quantumcatch_jerlovIb_50m<-green$quantum_catch_OM/qc_jerlovIb_50m$qc_jerlovIb_50m
white$quantumcatch_jerlovIb_50m<-white$quantum_catch_OM/qc_jerlovIb_50m$qc_jerlovIb_50m
red$quantumcatch_jerlovIb_50m<-red$quantum_catch_OM/qc_jerlovIb_50m$qc_jerlovIb_50m
qc_final_jerlovIb<-rbind(royalblue,blue,cyan,green,amber,red,white)

##jerlov ib 100m 

jerlovIb_100m<-mapply(ocularmedia1,FUN= function(x) x*jerlovIb$irradiance_100m)
jerlovIb_100m<-as.data.frame(jerlovIb_100m)
qc_jerlovIb_100m<-colSums(jerlovIb_100m)
qc_jerlovIb_100m<-as.data.frame(qc_jerlovIb_100m)
##add new column with row name filled, so now dataframe has 2 columns
qc_jerlovIb_100m<-setNames(cbind(rownames(qc_jerlovIb_100m), qc_jerlovIb_100m, row.names = NULL), 
                           c("species", "qc_jerlovIb_100m"))


##divide by quantum catch background
amber$quantumcatch_jerlovIb_100m=amber$quantum_catch_OM/qc_jerlovIb_100m$qc_jerlovIb_100m
royalblue$quantumcatch_jerlovIb_100m=royalblue$quantum_catch_OM/qc_jerlovIb_100m$qc_jerlovIb_100m
blue$quantumcatch_jerlovIb_100m<-blue$quantum_catch_OM/qc_jerlovIb_100m$qc_jerlovIb_100m
cyan$quantumcatch_jerlovIb_100m<-cyan$quantum_catch_OM/qc_jerlovIb_100m$qc_jerlovIb_100m
green$quantumcatch_jerlovIb_100m<-green$quantum_catch_OM/qc_jerlovIb_100m$qc_jerlovIb_100m
white$quantumcatch_jerlovIb_100m<-white$quantum_catch_OM/qc_jerlovIb_100m$qc_jerlovIb_100m
red$quantumcatch_jerlovIb_100m<-red$quantum_catch_OM/qc_jerlovIb_100m$qc_jerlovIb_100m
qc_final_jerlovIb<-rbind(royalblue,blue,cyan,green,amber,red,white)

####for each species, sum  each column and divide by length of each species group

library(dplyr)

##III 100m
qc_final_jerlov_new <- qc_final_jerlovIb %>%
  group_by(species, colour) %>%
  mutate(qc_total_III_100m=sum(quantumcatch_jerlovIII_100m)/length(species))
#III 50m
qc_final_jerlov_new2 <- qc_final_jerlov_new %>%
  group_by(species, colour) %>%
  mutate(qc_total_III_50m=sum(quantumcatch_jerlovIII_50m)/length(species))

#III 20m
qc_final_jerlov_new2.3 <- qc_final_jerlov_new2 %>%
  group_by(species, colour) %>%
  mutate(qc_total_III_20m=sum(quantumcatch_jerlovIII_20m)/length(species))

##IB 50m
qc_final_jerlov_new3 <- qc_final_jerlov_new2.3 %>%
  group_by(species, colour) %>%
  mutate(qc_total_Ib_50m=sum(quantumcatch_jerlovIb_50m)/length(species))

#IB 100M
qc_final_jerlov_new4.2 <- qc_final_jerlov_new3 %>%
  group_by(species, colour) %>%
  mutate(qc_total_Ib_100m=sum(quantumcatch_jerlovIb_100m)/length(species))
#IB 20M

qc_final_jerlov_new4.3 <- qc_final_jerlov_new4.2 %>%
  group_by(species, colour) %>%
  mutate(qc_total_Ib_20m=sum(quantumcatch_jerlovIb_20m)/length(species))

###reduce down to one column row?
##unique colour column

qc_final_jerlov_new32<-qc_final_jerlov_new4.3[!duplicated(qc_final_jerlov_new4.3$colour), ]

qc_final_jerlov_new32<-qc_final_jerlov_new4.3%>% distinct(colour, .keep_all = TRUE)


library(dplyr)
#qc_final_jerlovIb$qc_irradiance_20m_Ib<- paste(qc_jerlovIb_20m$qc_jerlovIb_20m)
#qc_final_jerlovIb$qc_irradiance_50m_Ib<- paste(qc_jerlovIb_50m$qc_jerlovIb_50m)
#qc_final_jerlovIb$qc_irradiance_20m_III<- paste(qc_jerlovIII_20m$qc_jerlovIII_20m)
#qc_final_jerlovIb$qc_irradiance_50m_III<- paste(qc_jerlovIII_50m$qc_jerlovIII_50m)
#qc_final_jerlovIb$qc_irradiance_20m_3c<- paste(qc_jerlov3C_20m$qc_jerlov3C_20m)
#qc_final_jerlovIb$qc_irradiance_50m_3c<- paste(qc_jerlov3C_50m$qc_jerlov3C_50m)
#write.csv(qc_final_jerlovIb, "quantum_catch__final_values.csv")

#df_transpose = t(qc_final_jerlovIb)

###Final data frame

library(ggplot2)
#walleye
walleye<-subset(qc_final_jerlov_new32, qc_final_jerlov_new32$species=="walleyepollock")
#salmon
salmon<-subset(qc_final_jerlov_new32, qc_final_jerlov_new32$species=="salmon")
#tuna
tuna<-subset(qc_final_jerlov_new32, qc_final_jerlov_new32$species=="yellowfintuna")
#hammerhead
hammerhead<-subset(qc_final_jerlov_new32, qc_final_jerlov_new32$species=="juvenilehammerhead")
sole<-subset(qc_final_jerlov_new32, qc_final_jerlov_new32$species=="sole")
thornbackray<-subset(qc_final_jerlov_new32, qc_final_jerlov_new32$species=="thornbackray")
nephrops<-subset(qc_final_jerlov_new32, qc_final_jerlov_new32$species=="nephrops")
cod<-subset(qc_final_jerlov_new32, qc_final_jerlov_new32$species=="cod")



#######CHAPTER 1 - Visual modelling case studies##########
##Alaska pollock and salmon
##Berring sea, ocean type III (according to Jerlov 1968)
##depths between 330 - 980ft (<100m)
##have data on 100m and 200m
library(ggpubr)
library(scales)

walleye_salmon<-rbind(walleye, salmon)
##100M turbid ocean
colourplotIII<-ggplot(walleye_salmon, aes(fill=species, y=qc_total_III_100m, x=colour)) +
  geom_bar(position="dodge", stat="identity")+
  labs(x = "LED colour modes", 
       y = "Visual stimulation at 100m in turbid ocean (III)",
       fill= "Species")+
  theme_bw()+
  theme(text = element_text(size=11))+
  theme(axis.text.x = element_text(size = 11))+   
  scale_x_discrete(labels = label_wrap(11)) +  theme(text = element_text(size=11))+
  scale_x_discrete(labels=c('Amber', 'Blue', 'Cyan', 'Green', 'Red','Royal blue', 'White'))+
  scale_fill_grey(start = 0.5, end=0.7, labels=c('Salmon', 'Alaska pollock'))
colourplotIII
##salmon is more visually stimulated than pollock

## less turbid water 100m IB
colourplotIb<-ggplot(walleye_salmon, aes(fill=species, y=qc_total_Ib_100m, x=colour)) +
  geom_bar(position="dodge", stat="identity")+
  labs(x = "LED colour modes", 
       y = "Visual stimulation at 100m in clear ocean (IB)",
       fill= "Species")+
  theme_bw()+
  theme(text = element_text(size=11))+
  theme(axis.text.x = element_text(size = 11))+   
  scale_x_discrete(labels = label_wrap(11)) +  theme(text = element_text(size=11))+
  scale_x_discrete(labels=c('Amber', 'Blue', 'Cyan', 'Green', 'Red','Royal blue', 'White'))+
  scale_fill_grey(start = 0.5, end=0.7, labels=c('Salmon', 'Alaska pollock'))
colourplotIb

ggarrange(colourplotIII,colourplotIb, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

###case study 2 - brazillian longlines scalloped hammerhead and tuna
##open water 100m

tuna_hammerhead<-rbind(tuna, hammerhead)
#both
colourplotIII<-ggplot(tuna_hammerhead, aes(fill=species, y=qc_total_III_100m, x=colour)) +
  geom_bar(position="dodge", stat="identity")+
  labs(x = "LED colour modes", 
       y = "Visual stimulation at 100m in turbid ocean (III)",
       fill= "Species")+
  theme_bw()+
  theme(text = element_text(size=11))+
  theme(axis.text.x = element_text(size = 11))+   
  scale_x_discrete(labels = label_wrap(11)) +  theme(text = element_text(size=11))+
  scale_x_discrete(labels=c('Amber', 'Blue', 'Cyan', 'Green', 'Red','Royal blue', 'White'))+
  scale_fill_grey(start = 0.5, end=0.7, labels=c('Scalloped hammerhead', 'Yellowfin tuna'))
colourplotIII

#ib 100
colourplotIb<-ggplot(tuna_hammerhead, aes(fill=species, y=qc_total_Ib_100m, x=colour)) +
  geom_bar(position="dodge", stat="identity")+
  labs(x = "LED colour modes", 
       y = "Visual stimulation at 100m in clear ocean (IB)",
       fill= "Species")+
  theme_bw()+
  theme(text = element_text(size=11))+
  theme(axis.text.x = element_text(size = 11))+   
  scale_x_discrete(labels = label_wrap(11)) +  theme(text = element_text(size=11))+
  scale_x_discrete(labels=c('Amber', 'Blue', 'Cyan', 'Green', 'Red','Royal blue', 'White'))+
  scale_fill_grey(start = 0.5, end=0.7, labels=c('Scalloped hammerhead', 'Yellowfin tuna'))
colourplotIb


ggarrange(colourplotIII,colourplotIb, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

##skate and sole
##50m IB - open ocean, not the clearest
thornbackray_sole<-rbind(thornbackray, sole)

#both
colourplotIb<-ggplot(thornbackray_sole, aes(fill=species, y=qc_total_Ib_50m, x=colour)) +
  geom_bar(position="dodge", stat="identity")+
  labs(x = "LED colour modes", 
       y = "Visual stimulation at 
   50m in clear ocean (IB)",
       fill= "Species")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 9))+   
  theme(text = element_text(size=11))+
  scale_x_discrete(labels = label_wrap(10)) +  theme(text = element_text(size=11))+
  scale_x_discrete(labels=c('Amber', 'Blue', 'Cyan', 'Green', 'Red','Royal blue', 'White'))+
  scale_fill_grey(start = 0.7, end=0.5, labels=c('Sole', 'Thornback ray'))
colourplotIb

colourplotIII<-ggplot(thornbackray_sole, aes(fill=species, y=qc_total_III_50m, x=colour)) +
  geom_bar(position="dodge", stat="identity")+
  labs(x = "LED colour modes", 
       y = "Visual stimulation at 
   50m in turbid ocean (III)",
       fill= "Species")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 9))+   
  theme(text = element_text(size=11))+
  scale_x_discrete(labels = label_wrap(10)) +  theme(text = element_text(size=11))+
  scale_x_discrete(labels=c('Amber', 'Blue', 'Cyan', 'Green', 'Red','Royal blue', 'White'))+
  scale_fill_grey(start = 0.7, end=0.5, labels=c('Sole', 'Thornback ray'))
colourplotIII


colourplotIII2<-ggplot(thornbackray_sole, aes(fill=species, y=qc_total_III_20m, x=colour)) +
  geom_bar(position="dodge", stat="identity")+
  labs(x = "LED colour modes", 
       y = "Visual stimulation at 
   20m in turbid ocean (III)",
       fill= "Species")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 9))+   
  theme(text = element_text(size=11))+
  scale_x_discrete(labels = label_wrap(10)) +  theme(text = element_text(size=11))+
  scale_x_discrete(labels=c('Amber', 'Blue', 'Cyan', 'Green', 'Red','Royal blue', 'White'))+
  scale_fill_grey(start = 0.7, end=0.5, labels=c('Sole', 'Thornback ray'))
colourplotIII2

colourplotIb2<-ggplot(thornbackray_sole, aes(fill=species, y=qc_total_Ib_20m, x=colour)) +
  geom_bar(position="dodge", stat="identity")+
  labs(x = "LED colour modes", 
       y = "Visual stimulation at 
   20m in clear ocean (IB)",
       fill= "Species")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 9))+   
  theme(text = element_text(size=11))+
  scale_x_discrete(labels = label_wrap(10)) +  theme(text = element_text(size=11))+
  scale_x_discrete(labels=c('Amber', 'Blue', 'Cyan', 'Green', 'Red','Royal blue', 'White'))+
  scale_fill_grey(start = 0.7, end=0.5, labels=c('Sole', 'Thornback ray'))
colourplotIb2


colourplot3c<-ggplot(thornbackray_sole, aes(fill=species, y=qc_total_3C_20m, x=colour)) +
  geom_bar(position="dodge", stat="identity")+
  labs(x = "LED colour modes", 
       y = "Visual stimulation at 
   20m in coastal waters",
       fill= "Species")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 7))+   
  theme(text = element_text(size=11))+
  scale_x_discrete(labels = label_wrap(10)) +  theme(text = element_text(size=11))+
  scale_x_discrete(labels=c('Amber', 'Blue', 'Cyan', 'Green', 'Red','Royal blue', 'White'))+
  scale_fill_grey(start = 0.7, end=0.5, labels=c('Sole', 'Thornback ray'))
colourplot3c

ggarrange(colourplotIII,colourplotIII2, colourplotIb,colourplotIb2, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

##nephrops
nephrops<-subset(qc_final_jerlov_new32, qc_final_jerlov_new32$species=="nephrops")
cod<-subset(qc_final_jerlov_new32, qc_final_jerlov_new32$species=="cod")

cod_nephrops<-rbind(cod, nephrops)
#both
colourplotIII<-ggplot(cod_nephrops, aes(fill=species, y=qc_total_III_100m, x=colour)) +
  geom_bar(position="dodge", stat="identity")+
  labs(x = "LED colour modes", 
       y = "Visual stimulation at 100m in turbid ocean (III)",
       fill= "Species")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 11))+   
  scale_x_discrete(labels = label_wrap(11)) +  theme(text = element_text(size=11))+
  scale_x_discrete(labels=c('Amber', 'Blue', 'Cyan', 'Green', 'Red','Royal blue', 'White'))+
  scale_fill_grey(start = 0.5, end=0.7, labels=c('Cod', 'Nephrops'))
colourplotIII

#IB
colourplotIb<-ggplot(cod_nephrops, aes(fill=species, y=qc_total_Ib_100m, x=colour)) +
  geom_bar(position="dodge", stat="identity")+
  labs(x = "LED colour modes", 
       y = "Visual stimulation at 100m in clear ocean (IB)",
       fill= "Species")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 11))+   
  scale_x_discrete(labels = label_wrap(11)) +  theme(text = element_text(size=11))+
  scale_x_discrete(labels=c('Amber', 'Blue', 'Cyan', 'Green', 'Red','Royal blue', 'White'))+
  scale_fill_grey(start = 0.5, end=0.7, labels=c('Cod', 'Nephrops'))
colourplotIb

ggarrange(colourplotIII,colourplotIb, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
