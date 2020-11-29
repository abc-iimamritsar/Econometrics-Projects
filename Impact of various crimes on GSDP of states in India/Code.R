setwd("C:/Users/asnis/Desktop/IIM Amritsar/Term 4/Econometrics/Group Project")
library(plm)
library(readxl)
library(broom)
library(stargazer)
library(lmtest)


crimedata <- read_excel("final dataset.xlsx", sheet = "Sheet1")
pdata= pdata.frame(crimedata,index = c("STATEUT","YEAR"))

pooledmethod= plm(GDPgrowthrate~Inflation+MurderHomicidepercapita+RAPEpercapita+KIDNAPPINGandABDUCTIONpercapita+DACOITYRobberyTheftpercapita+RIOTSpercapita+CRIMINALBREACHOFTRUSTpercapita+CHEATINGpercapita+COUNTERFIETINGpercapita+ARSONpercapita+HURTGREVIOUSHURTpercapita+ASSAULTONWOMENpercapita+CAUSINGDEATHBYNEGLIGENCEpercapita+OTHERIPCCRIMESpercapita,data=pdata,model="pooling")
summary(pooledmethod)
stargazer(pooledmethod,out = "pooledmethod.txt")

femethod= plm(GDPgrowthrate~Inflation+MurderHomicidepercapita+RAPEpercapita+KIDNAPPINGandABDUCTIONpercapita+DACOITYRobberyTheftpercapita+RIOTSpercapita+CRIMINALBREACHOFTRUSTpercapita+CHEATINGpercapita+COUNTERFIETINGpercapita+ARSONpercapita+HURTGREVIOUSHURTpercapita+ASSAULTONWOMENpercapita+CAUSINGDEATHBYNEGLIGENCEpercapita+OTHERIPCCRIMESpercapita,data=pdata,model="within")
summary(femethod)
stargazer(femethod,out = "femethod.txt")

remethod= plm(GDPgrowthrate~Inflation+MurderHomicidepercapita+RAPEpercapita+KIDNAPPINGandABDUCTIONpercapita+DACOITYRobberyTheftpercapita+RIOTSpercapita+CRIMINALBREACHOFTRUSTpercapita+CHEATINGpercapita+COUNTERFIETINGpercapita+ARSONpercapita+HURTGREVIOUSHURTpercapita+ASSAULTONWOMENpercapita+CAUSINGDEATHBYNEGLIGENCEpercapita+OTHERIPCCRIMESpercapita,data=pdata, model="random")
summary(remethod)
stargazer(remethod,out = "remethod.txt")


#this command is not working because the number of independent variables (col) is more than the number of time preriod (10) 
pooltest(GDPgrowthrate~Inflation+MurderHomicidepercapita+RAPEpercapita+KIDNAPPINGandABDUCTIONpercapita+DACOITYRobberyTheftpercapita+RIOTSpercapita+CRIMINALBREACHOFTRUSTpercapita+CHEATINGpercapita+COUNTERFIETINGpercapita+ARSONpercapita+HURTGREVIOUSHURTpercapita+ASSAULTONWOMENpercapita+CAUSINGDEATHBYNEGLIGENCEpercapita+OTHERIPCCRIMESpercapita,data=pdata, model= "within")

# on less number of the independent variables, the conclusion is that there is stability in pooledmethod
pooltest(GDPgrowthrate~Inflation+MurderHomicidepercapita+RAPEpercapita+KIDNAPPINGandABDUCTIONpercapita+DACOITYRobberyTheftpercapita+RIOTSpercapita+CRIMINALBREACHOFTRUSTpercapita+CHEATINGpercapita,data=pdata, model= "within")

# command to test presence of individual and time effects. Conclusion is presence of significant effects
plmtest(GDPgrowthrate~Inflation+MurderHomicidepercapita+RAPEpercapita+KIDNAPPINGandABDUCTIONpercapita+DACOITYRobberyTheftpercapita+RIOTSpercapita+CRIMINALBREACHOFTRUSTpercapita+CHEATINGpercapita+COUNTERFIETINGpercapita+ARSONpercapita+HURTGREVIOUSHURTpercapita+ASSAULTONWOMENpercapita+CAUSINGDEATHBYNEGLIGENCEpercapita+OTHERIPCCRIMESpercapita, data = pdata,effect = "twoways",type = "ghm")

#the conclusion from this command is that Fixed effects model is consistent for this data and pooled OLS is rejected
pFtest(femethod,pooledmethod)

#Hausman test, conclusion is that random effects model is consistent for our data and fixed effects model is not
phtest(femethod,remethod)

#Unit root test (h0 = unit root present that is variable is non stationary) conclusion - stationarity is present
checkInflation = data.frame(split(crimedata$Inflation, crimedata$STATEUT))
purtest(checkInflation, pmax = 2, exo = "intercept", test = "levinlin")

#Unit root test (h0 = unit root present that is variable is non stationary) conclusion - stationarity is present
checkMurderHomicidepercapita = data.frame(split(crimedata$MurderHomicidepercapita, crimedata$STATEUT))
purtest(checkMurderHomicidepercapita, pmax = 2, exo = "intercept", test = "levinlin")

#Unit root test (h0 = unit root present that is variable is non stationary) conclusion - stationarity is present
checkRAPEpercapita = data.frame(split(crimedata$RAPEpercapita, crimedata$STATEUT))
purtest(checkRAPEpercapita, pmax = 2, exo = "intercept", test = "levinlin")

#Unit root test (h0 = unit root present that is variable is non stationary) conclusion - stationarity is present
checkKIDNAPPINGandABDUCTIONpercapita = data.frame(split(crimedata$KIDNAPPINGandABDUCTIONpercapita, crimedata$STATEUT))
purtest(checkKIDNAPPINGandABDUCTIONpercapita, pmax = 2, exo = "intercept", test = "levinlin")

#Unit root test (h0 = unit root present that is variable is non stationary) conclusion - stationarity is present
checkDACOITYRobberyTheftpercapita = data.frame(split(crimedata$DACOITYRobberyTheftpercapita, crimedata$STATEUT))
purtest(checkDACOITYRobberyTheftpercapita, pmax = 2, exo = "intercept", test = "levinlin")

#Unit root test (h0 = unit root present that is variable is non stationary) conclusion - stationarity is present
checkRIOTSpercapita = data.frame(split(crimedata$RIOTSpercapita, crimedata$STATEUT))
purtest(checkRIOTSpercapita, pmax = 2, exo = "intercept", test = "levinlin")

#Unit root test (h0 = unit root present that is variable is non stationary) conclusion - stationarity is present
checkCRIMINALBREACHOFTRUSTpercapita = data.frame(split(crimedata$CRIMINALBREACHOFTRUSTpercapita, crimedata$STATEUT))
purtest(checkCRIMINALBREACHOFTRUSTpercapita, pmax = 2, exo = "intercept", test = "levinlin")

#Unit root test (h0 = unit root present that is variable is non stationary) conclusion - stationarity is present
checkCHEATINGpercapita = data.frame(split(crimedata$CHEATINGpercapita, crimedata$STATEUT))
purtest(checkCHEATINGpercapita, pmax = 2, exo = "intercept", test = "levinlin")

#Unit root test (h0 = unit root present that is variable is non stationary) conclusion - stationarity is present
checkCHEATINGpercapita = data.frame(split(crimedata$CHEATINGpercapita, crimedata$STATEUT))
purtest(checkCHEATINGpercapita, pmax = 2, exo = "intercept", test = "levinlin")

#Unit root test (h0 = unit root present that is variable is non stationary) conclusion - stationarity is present
checkCOUNTERFIETINGpercapita = data.frame(split(crimedata$COUNTERFIETINGpercapita, crimedata$STATEUT))
purtest(checkCOUNTERFIETINGpercapita, pmax = 2, exo = "intercept", test = "levinlin")

#Unit root test (h0 = unit root present that is variable is non stationary) conclusion - stationarity is present
checkARSONpercapita = data.frame(split(crimedata$ARSONpercapita, crimedata$STATEUT))
purtest(checkARSONpercapita, pmax = 2, exo = "intercept", test = "levinlin")

#Unit root test (h0 = unit root present that is variable is non stationary) conclusion - stationarity is present
checkHURTGREVIOUSHURTpercapita = data.frame(split(crimedata$HURTGREVIOUSHURTpercapita, crimedata$STATEUT))
purtest(checkHURTGREVIOUSHURTpercapita, pmax = 2, exo = "intercept", test = "levinlin")

#Unit root test (h0 = unit root present that is variable is non stationary) conclusion - stationarity is present
checkASSAULTONWOMENpercapita = data.frame(split(crimedata$ASSAULTONWOMENpercapita, crimedata$STATEUT))
purtest(checkASSAULTONWOMENpercapita, pmax = 2, exo = "intercept", test = "levinlin")

#Unit root test (h0 = unit root present that is variable is non stationary) conclusion - stationarity is present
checkCAUSINGDEATHBYNEGLIGENCEpercapita = data.frame(split(crimedata$CAUSINGDEATHBYNEGLIGENCEpercapita, crimedata$STATEUT))
purtest(checkCAUSINGDEATHBYNEGLIGENCEpercapita, pmax = 2, exo = "intercept", test = "levinlin")

#Unit root test (h0 = unit root present that is variable is non stationary) conclusion - stationarity is present
checkOTHERIPCCRIMESpercapita = data.frame(split(crimedata$OTHERIPCCRIMESpercapita, crimedata$STATEUT))
purtest(checkOTHERIPCCRIMESpercapita, pmax = 2, exo = "intercept", test = "levinlin")

#durbin watson test to check autocorrelation in error term (ho is - no autocorrelation is error term) conclusion is- there is serial correlation in idiosyncretic errors
pdwtest(GDPgrowthrate~Inflation+MurderHomicidepercapita+RAPEpercapita+KIDNAPPINGandABDUCTIONpercapita+DACOITYRobberyTheftpercapita+RIOTSpercapita+CRIMINALBREACHOFTRUSTpercapita+CHEATINGpercapita+COUNTERFIETINGpercapita+ARSONpercapita+HURTGREVIOUSHURTpercapita+ASSAULTONWOMENpercapita+CAUSINGDEATHBYNEGLIGENCEpercapita+OTHERIPCCRIMESpercapita,data = pdata,model = "random")

#testing homoskadasticity (ho is - there is homoskadasticity) conclusion- there is heteroskadasticity in the dataset
bptest(GDPgrowthrate~Inflation+MurderHomicidepercapita+RAPEpercapita+KIDNAPPINGandABDUCTIONpercapita+DACOITYRobberyTheftpercapita+RIOTSpercapita+CRIMINALBREACHOFTRUSTpercapita+CHEATINGpercapita+COUNTERFIETINGpercapita+ARSONpercapita+HURTGREVIOUSHURTpercapita+ASSAULTONWOMENpercapita+CAUSINGDEATHBYNEGLIGENCEpercapita+OTHERIPCCRIMESpercapita, data = pdata, studentize = F)

#controlling for heterskadasticity and autocorrelation by calculating roboust SE
coeftest(femethod,vcovHC(femethod,method = "arellano"))

#New Model
edithedremethod= plm(GDPgrowthrate~Inflation+MurderHomicidepercapita+ASSAULTONWOMENpercapita+CAUSINGDEATHBYNEGLIGENCEpercapita,data=pdata, model="random")
summary(edithedremethod)
