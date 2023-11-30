# GISD - German Index of Socio-Economic Deprivation
# Author: 
# Citation: 
# https://github.com/robert-koch-institut/German_Index_of_Socioeconomic_Deprivation_GISD

# Revision: 2022.v3
# Date: 2022-11-22

# Libraries
## DW
## nur benötigte Pakete laden
library(dplyr)
library(tidyr)
library(readxl)
library(haven)
library(sf)
# library(pastecs)  # wird nur 1x für stat.desc() benötigt

## DW
## Arbeitsverzeichnis auf Skriptverzeichnis setzen
# setwd("/path/to/file")
getwd()

# Path Definitions
## DW
## relative Pfade verwenden
## alle Ordner hier erstellen
infiles_dir  <- "../Rohdaten/"
outfiles_dir <- "../Outfiles/"
dir.create(outfiles_dir,                                showWarnings=TRUE)
dir.create(paste0(outfiles_dir, "2022_v03"),            showWarnings=TRUE)
dir.create(paste0(outfiles_dir, "2022_v03/Bund"),       showWarnings=TRUE)
dir.create(paste0(outfiles_dir, "2022_v03/Bundesland"), showWarnings=FALSE)
dir.create(paste0(outfiles_dir, "2022_v03/Other"),      showWarnings=TRUE)
dir.create(paste0(outfiles_dir, "2022_v03/Stata"),      showWarnings=TRUE)

## I.  Generierung eines ID-Datensatzes
load_dataset <- function(sheet) {
    suppressMessages(
        read_excel(paste0(infiles_dir, "Referenz/Referenz_1998_2019.xls"),
                   sheet=sheet, na="NA")
    )
}

Gemeinden_INKAR <- load_dataset("Gemeinden-GVB") %>% 
    na.omit() %>%
    rename(GVBKennziffer=gvb19) %>%
    mutate(Kennziffer=as.numeric(gem19),
           fl19      =as.numeric(fl19)) %>%
    select(-gem19)

Gemeindeverbaende_INKAR <- load_dataset("Gemeindeverbände") %>% 
    na.omit() %>% 
    select(GVBKennziffer=gvb19,
           "Name des Gemeindeverbands"=gvb19name)

Kreise_INKAR <- load_dataset("KRS") %>%
    mutate(krs19=as.numeric(krs19),
           fl19 =as.numeric(fl19))

id_dataset <- Gemeinden_INKAR %>% 
    select(Gemeindekennziffer=Kennziffer,
           "Name der Gemeinde"=gem19name,
           GVBKennziffer,
           Bevoelkerung=Bevölkerung) %>% 
    mutate(Kreiskennziffer=floor(Gemeindekennziffer/1000)) %>%
    left_join(Kreise_INKAR %>% select(Kreiskennziffer=krs19,
                                      "Name des Kreises"=krs19name,
                                      "Raumordnungsregion Nr"=ROR11,
                                      Raumordnungsregion=ROR11name,
                                      NUTS2,
                                      "NUTS2 Name"=NUTS2name,
                                      Bundesland),
              by="Kreiskennziffer") %>%
    left_join(Gemeindeverbaende_INKAR, by="GVBKennziffer")

## II. Erzeugen eines Datensatzes mit Kennziffern als ID unabhängig von der Ebene
Basedata <- Kreise_INKAR %>%
    select(Kennziffer=krs19) %>%
    mutate(Jahr=2019)

inputdataset1 <- list.files(paste0(infiles_dir, "INKAR_1998_2019/"),
                            pattern="\\.xlsx$")
inputdataset1

for(file in inputdataset1) {
    suppressMessages(myimport <- read_excel(paste0(infiles_dir, "INKAR_1998_2019/", file),
                                            skip=1, sheet="Daten"))
    names(myimport)[1] <- "Kennziffer"
    myimport[2:3] <- NULL
    myimport <- myimport %>%
        gather(key=Jahr,
               value=Value,
               -Kennziffer,
               convert=TRUE,
               na.rm=TRUE) %>%
        mutate(Kennziffer=as.numeric(as.character(Kennziffer)),
               Value=as.numeric(Value))
  
    names(myimport)[names(myimport) == "Value"] <- gsub("^.+_(.+)[.].+$", "\\1", file)
    Basedata <- Basedata %>%
        full_join(myimport, by=c("Kennziffer", "Jahr"))
}

rm(inputdataset1, myimport)

# Tabelle der Indikatoren mit regionaler Tiefe
level_table <- data.frame(Indikator=names(Basedata)[-(1:2)],
                          Tiefe_Indikator=c("Gemeindeverband", "Gemeindeverband",
                                            "Kreis", "Gemeindeverband",
                                            "Kreis", "Kreis", "Kreis", "Kreis", "Kreis",
                                            "Gemeindeverband", "Kreis", "Kreis"))

Indikatoren_Gemeindeverband <- level_table %>%
    filter(Tiefe_Indikator == "Gemeindeverband") %>%
    pull(Indikator)

Indikatoren_Kreis <- level_table %>%
    filter(Tiefe_Indikator == "Kreis") %>%
    pull(Indikator)

Basedata_Gemeindeverbandsebene <- Basedata %>%
    select(Gemeindeverband=Kennziffer,
           Jahr,
           all_of(Indikatoren_Gemeindeverband)) %>%
    ## DW
    ## soll Einkommensteuer (Spalte 6) hier tatsächlich ausgenommen sein?
    gather(key, value, 3:5) %>%
    filter(!is.na(value)) %>%
    spread(key, value) %>%
    filter(Jahr >= 1998)

Basedata_Kreisebene <- Basedata %>%
    select(Kreis=Kennziffer,
           Jahr,
           all_of(Indikatoren_Kreis)) %>%
    filter(Jahr >= 1998)

Workfile <- expand.grid(Kennziffer=Gemeinden_INKAR$Kennziffer,
                        Jahr=sort(unique(Basedata$Jahr))) %>%
    mutate(Kreiskennziffer=floor(as.numeric(Kennziffer)/1000)) %>%
    as_tibble() %>%
    left_join(Gemeinden_INKAR, by="Kennziffer") %>%
    select(Gemeindekennziffer=Kennziffer,
           Kreis=Kreiskennziffer,
           Gemeindeverband=GVBKennziffer,
           Jahr,
           Bevoelkerung=Bevölkerung) %>%
    mutate(Gemeindeverband=as.numeric(Gemeindeverband),
           Bevoelkerung=as.numeric(Bevoelkerung)) %>% 
    arrange(Gemeindeverband, Jahr) %>% # Join Metadata
    left_join(Basedata_Kreisebene, by=c("Kreis", "Jahr")) %>%
    left_join(Basedata_Gemeindeverbandsebene, by=c("Gemeindeverband", "Jahr")) %>%
    filter(Jahr >= 1998)

inputdataset2 <- list.files(paste0(infiles_dir, "INKAR_1998_2019/Indikatoren_Kreisebene/"))

for(file in inputdataset2){
    suppressMessages(myimport <- read_excel(paste0(infiles_dir, "INKAR_1998_2019/Indikatoren_Kreisebene/", file),
                                            skip=1, sheet="Daten"))
    names(myimport)[1] <- "Kreis"
    myimport[2:3] <- NULL
    myimport <- myimport %>%
        gather(key=Jahr,
               value=Value,
               -Kreis,
               convert=TRUE,
               na.rm=TRUE) %>%
        mutate(Kreis=as.numeric(as.character(Kreis)),
               Value=as.numeric(Value))
    
    names(myimport)[names(myimport) == "Value"] <- gsub("^.+_(.+)[.].+$", "\\1", file)
    Workfile <- Workfile %>%
        full_join(myimport, by=c("Kreis", "Jahr"))
}

rm(inputdataset2, myimport)

Workfile <- Workfile %>%
    mutate(ErwerbsfaehigeBevoelkerung=ifelse(Jahr < 2001, ErwerbsfaehigeBevoelkerungKreis, ErwerbsfaehigeBevoelkerung),
           Arbeitslosigkeit          =ifelse(Jahr < 2001, ArbeitslosigkeitKreis,           Arbeitslosigkeit),
           Beschaeftigtenquote       =ifelse(Jahr < 2001, BeschaeftigtenquoteKreis,        Beschaeftigtenquote)) %>%
    select(-ErwerbsfaehigeBevoelkerungKreis,
           -ArbeitslosigkeitKreis,
           -BeschaeftigtenquoteKreis)

Gemeinden_ohne_Bevoelkerung <- Workfile %>%
    filter(Bevoelkerung == 0)

write_dta(Gemeinden_ohne_Bevoelkerung,
          paste0(outfiles_dir, "Gemeinden_ohne_Bevoelkerung.dta"))
rm(Gemeinden_ohne_Bevoelkerung)

Workfile <- Workfile %>%
    filter(Bevoelkerung > 0) %>%
    mutate(BeschaeftigteohneAbschluss    =round(BeschaeftigteohneAbschluss    / SVBeschaeftigte * 100, digits=2),
           BeschaeftigtemitakadAbschluss =round(BeschaeftigtemitakadAbschluss / SVBeschaeftigte * 100, digits=2),
           Arbeitslosigkeit=round(Arbeitslosigkeit / ErwerbsfaehigeBevoelkerung * 1000, digits=2),
           Arbeitslosigkeit=ifelse(is.finite(Arbeitslosigkeit),
                                   Arbeitslosigkeit, NA)) %>%
    select(-SVBeschaeftigte, -ErwerbsfaehigeBevoelkerung)

Basedata <- Basedata %>%
    select(-SVBeschaeftigte, -ErwerbsfaehigeBevoelkerung)

Basedata_Kreisebene <- Basedata_Kreisebene %>%
    select(-SVBeschaeftigte)

Basedata_Gemeindeverbandsebene <- Basedata_Gemeindeverbandsebene %>%
    select(-ErwerbsfaehigeBevoelkerung)

## III.Imputation fehlender Werte
Verbraucherpreisindex <- data.frame(Jahr   =c(1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018,  2019,  2020),
                                    VBindex=c(78.3, 78.8, 79.9, 81.5, 82.6, 83.5, 84.9, 86.2, 87.6, 89.6, 91.9, 92.2, 93.2, 95.2, 97.1, 98.5, 99.5, 100, 100.5, 102, 103.8, 105.3, 105.8))

Workfile <- Workfile %>%
    left_join(Verbraucherpreisindex, by="Jahr") %>%
    mutate(Einkommensteuer   =Einkommensteuer    / VBindex * 100,
           Haushaltseinkommen=Haushaltseinkommen / VBindex * 100,
           Bruttoverdienst   =Bruttoverdienst    / VBindex * 100,
           Einkommensteuer_ln   =ifelse(Einkommensteuer == 0, 0.75, log(Einkommensteuer)),
           Haushaltseinkommen_ln=log(Haushaltseinkommen),
           Bruttoverdienst_ln   =log(Bruttoverdienst),
           G8_jahr=case_when(Kreis < 2000  &                 Jahr == 2016 ~ 1,
                             Kreis > 1999  & Kreis < 3000  & Jahr == 2010 ~ 1,
                             Kreis > 2999  & Kreis < 4000  & Jahr == 2011 ~ 1,
                             Kreis > 3999  & Kreis < 5000  & Jahr == 2012 ~ 1,
                             Kreis > 4999  & Kreis < 6000  & Jahr == 2013 ~ 1,
                             Kreis > 5999  & Kreis < 7000  & Jahr == 2013 ~ 1,
                             Kreis > 7999  & Kreis < 9000  & Jahr == 2012 ~ 1,
                             Kreis > 8999  & Kreis < 10000 & Jahr == 2011 ~ 1,
                             Kreis > 9999  & Kreis < 11000 & Jahr == 2009 ~ 1,
                             Kreis > 10999 & Kreis < 12000 & Jahr == 2012 ~ 1,
                             Kreis > 11999 & Kreis < 13000 & Jahr == 2012 ~ 1,
                             Kreis > 12999 & Kreis < 14000 & Jahr == 2008 ~ 1,
                             Kreis > 14999 & Kreis < 16000 & Jahr == 2007 ~ 1),
         SN_KA       =ifelse(Kreis > 14999 & Kreis < 16000 & Jahr == 2001, 1, 0),
         THvor2004   =ifelse(Kreis > 15999                 & Jahr <  2004, 1, 0),
         G8_jahr  =if_else(is.na(G8_jahr),   0, G8_jahr),
         SN_KA    =if_else(is.na(SN_KA),     0, SN_KA),
         THvor2004=if_else(is.na(THvor2004), 0, THvor2004))

adj_G8_jahr <- function(data, outcome_name) {
  mydata <- data %>%
      group_by(Gemeindekennziffer) %>% 
      select(Gemeindekennziffer, Jahr, G8_jahr, SN_KA, THvor2004,
             Outcome=all_of(outcome_name)) %>%
      mutate(MEAN=mean(Outcome, na.rm=TRUE)) %>%
      ungroup()
  
  mymodell2 <- lm(Outcome ~
                    I(Jahr*Jahr*MEAN) + I(Jahr*MEAN) + G8_jahr + SN_KA + THvor2004,
                  data=mydata, na.action="na.exclude")
  print(mymodell2)
  mydata %>%
      mutate(coef_G8=coef(mymodell2)["G8_jahr"],
             coef_SH=coef(mymodell2)["SN_KA"],
             coef_TH=coef(mymodell2)["THvor2004"],
             Outcome=ifelse(G8_jahr   == 1, Outcome - coef_G8, Outcome),
             Outcome=ifelse(SN_KA     == 1, Outcome - coef_SH, Outcome),
             Outcome=ifelse(THvor2004 == 1, Outcome - coef_TH, Outcome)) %>%
      pull(Outcome)
}

OW <- function(data, outcome_name) {
    mydata <- data %>%
        select(Gemeindekennziffer, Jahr, OW,
               Outcome=all_of(outcome_name)) %>% 
        mutate(Jahr_Dummy=relevel(as.factor(Jahr), ref="2012")) %>%
        ungroup()
    
    mymodell_ow <- lm(Outcome ~ Jahr_Dummy + Jahr_Dummy*OW,
                      data=mydata, na.action="na.exclude")
    
    print(mymodell_ow)
    coef(mymodell_ow)["OW"]
    mydata %>%
        mutate(coef_ow=coef(mymodell_ow)["OW"]) %>%
        mutate(Outcome=ifelse(OW == 1, Outcome - coef_ow, Outcome)) %>%
        pull(Outcome)
}

Workfile <- Workfile %>%
    mutate(SchulabgaengermitHochschulreife_adj=adj_G8_jahr(., "SchulabgaengermitHochschulreife"),
           SchulabgaengerohneAbschluss_adj    =adj_G8_jahr(., "SchulabgaengerohneAbschluss"),
           OW=ifelse(Kreis < 11000, 0, 1)) %>%
    mutate(BeschaeftigteohneAbschluss_adj=OW(., "BeschaeftigteohneAbschluss"))

Workfile_spread <- Workfile %>%
    filter(Jahr == 2013) %>%
    spread(key=Jahr,
           value=BeschaeftigteohneAbschluss_adj) %>%
    select("2013", Gemeindekennziffer) %>%
    mutate(Jahr=2012) %>%
    rename(BeschaeftigteohneAbschluss_adj="2013")

Workfile <- Workfile %>%
    left_join(Workfile_spread, by=c("Gemeindekennziffer", "Jahr")) %>%
    mutate(BeschaeftigteohneAbschluss_adj=ifelse(Jahr == 2012,
                                                 BeschaeftigteohneAbschluss_adj.y,
                                                 BeschaeftigteohneAbschluss_adj.x)) %>%
    select(-BeschaeftigteohneAbschluss_adj.y,
           -BeschaeftigteohneAbschluss_adj.x)

Workfile_spread <- Workfile %>%
    filter(Jahr == 2013) %>%
    spread(key=Jahr,
           value=BeschaeftigtemitakadAbschluss) %>%
    select("2013", Gemeindekennziffer) %>%
    mutate(Jahr=2012) %>%
    rename(BeschaeftigtemitakadAbschluss="2013")

Workfile <- Workfile %>%
    left_join(Workfile_spread, by=c("Gemeindekennziffer", "Jahr")) %>%
    mutate(BeschaeftigtemitakadAbschluss=ifelse(Jahr == 2012,
                                                BeschaeftigtemitakadAbschluss.y,
                                                BeschaeftigtemitakadAbschluss.x),
           Messaenderung_Besch=ifelse(Jahr > 2012, 1, 0)) %>%
    select(-BeschaeftigtemitakadAbschluss.y,
           -BeschaeftigtemitakadAbschluss.x)

Messaenderung <- function(data, outcome_name) {
    mydata <- data %>%
        select(Gemeindekennziffer, Jahr, Messaenderung_Besch,
               Outcome=all_of(outcome_name)) %>% 
        mutate(MEAN=mean(Outcome, na.rm=TRUE)) %>%
        ungroup()
  
    mymodell_Messaenderung <- lm(Outcome ~
                                   I(Jahr*Jahr*MEAN) + I(Jahr*MEAN) + Messaenderung_Besch,
                                 data=mydata, na.action="na.exclude")
  
    print(mymodell_Messaenderung) 
    coef(mymodell_Messaenderung)["Messaenderung_Besch"]
    mydata <- mydata %>% 
        mutate(coef_mb=coef(mymodell_Messaenderung)["Messaenderung_Besch"]) %>%
        mutate(Outcome=ifelse(Jahr < 2012, Outcome + coef_mb, Outcome)) %>%
        pull(Outcome)
}

Workfile <- Workfile %>% 
    mutate(BeschaeftigteohneAbschluss_adj   =Messaenderung(., "BeschaeftigteohneAbschluss_adj"),
           BeschaeftigtemitakadAbschluss_adj=Messaenderung(., "BeschaeftigtemitakadAbschluss"))

## DW
## Index 6:15 korrekt für diese Variablen?
## "Bruttoverdienst"                 "BeschaeftigteohneAbschluss"     
## "SchulabgaengermitHochschulreife" "SchulabgaengerohneAbschluss"    
## "Haushaltseinkommen"              "Schuldnerquote"                 
## "Einkommensteuer"                 "Arbeitslosigkeit"               
## "Beschaeftigtenquote"             "VBindex"

Impdata <-  Workfile %>%
    filter(Jahr >= 1998) %>% 
    gather(key, value, 6:15) %>%
    mutate(value=ifelse(value < 0.00001, NA, value)) %>%
    spread(key, value)

my_ts_imputer1 <- function(data, outcome_name) {
    mydata <- data %>%
        group_by(Gemeindekennziffer) %>%
        select(Gemeindekennziffer, Jahr,
               Outcome=all_of(outcome_name)) %>% 
        mutate(MEAN=mean(Outcome, na.rm=TRUE)) %>%
        ungroup()
    
    mymodell <- lm(Outcome ~
                     I(Jahr*Jahr*MEAN) + I(Jahr*MEAN),
                   data=mydata, na.action="na.exclude")
    
    mydata %>%
        mutate(Imputed=predict(mymodell, newdata=.)) %>%
        mutate(Outcome=ifelse(is.finite(Outcome), Outcome, Imputed),
               Outcome=ifelse(Outcome < 0, 0, Outcome)) %>%
        pull(Outcome)
}

Impdata.imputed <- Impdata %>%
  mutate(Arbeitslosigkeit                 =my_ts_imputer1(., "Arbeitslosigkeit"),
         SchulabgaengerohneAbschluss_adj  =my_ts_imputer1(., "SchulabgaengerohneAbschluss_adj"),
         SchulabgaengerohneAbschluss      =my_ts_imputer1(., "SchulabgaengerohneAbschluss"),
         Beschaeftigtenquote              =my_ts_imputer1(., "Beschaeftigtenquote"),
         Bruttoverdienst_ln               =my_ts_imputer1(., "Bruttoverdienst_ln"),
         Bruttoverdienst                  =my_ts_imputer1(., "Bruttoverdienst"),
         BeschaeftigtemitakadAbschluss_adj=my_ts_imputer1(., "BeschaeftigtemitakadAbschluss_adj"),
         BeschaeftigtemitakadAbschluss    =my_ts_imputer1(., "BeschaeftigtemitakadAbschluss"),
         BeschaeftigteohneAbschluss_adj   =my_ts_imputer1(., "BeschaeftigteohneAbschluss_adj"),
         BeschaeftigteohneAbschluss       =my_ts_imputer1(., "BeschaeftigteohneAbschluss"),
         Einkommensteuer_ln               =my_ts_imputer1(., "Einkommensteuer_ln"),
         Einkommensteuer                  =my_ts_imputer1(., "Einkommensteuer"),
         Haushaltseinkommen_ln            =my_ts_imputer1(., "Haushaltseinkommen_ln"),
         Haushaltseinkommen               =my_ts_imputer1(., "Haushaltseinkommen"),
         Schuldnerquote                   =my_ts_imputer1(., "Schuldnerquote"),
         ## DW
         ## Variablen nicht vorhanden
         # BevoelkerungohneAbschluss        =my_ts_imputer1(., "BevoelkerungohneAbschluss"),
         # BevoelkerungmitakadAbschluss     =my_ts_imputer1(., "BevoelkerungmitakadAbschluss")
         )

listofdeterminants <- c("Arbeitslosigkeit",
                        "SchulabgaengerohneAbschluss_adj",
                        "Beschaeftigtenquote",
                        "Bruttoverdienst_ln",
                        "BeschaeftigtemitakadAbschluss_adj",
                        "BeschaeftigteohneAbschluss_adj",
                        "Einkommensteuer_ln",
                        "Haushaltseinkommen_ln",
                        "Schuldnerquote",
                        ## DW
                        ## Variablen nicht vorhanden
                        # "BevoelkerungohneAbschluss",
                        # "BevoelkerungmitakadAbschluss",
                        "SchulabgaengermitHochschulreife_adj")

Impdata.imputed %>%
    as.data.frame() %>%
    select(all_of(listofdeterminants)) %>%
    summary()

my_ts_imputer2 <- function(data, outcome_name) {
    mydata <- data %>%
        select(Gemeindekennziffer, Jahr, Arbeitslosigkeit,
               SchulabgaengerohneAbschluss_adj,
               Beschaeftigtenquote, Bruttoverdienst_ln,
               BeschaeftigtemitakadAbschluss_adj,
               BeschaeftigteohneAbschluss_adj,
               Einkommensteuer_ln,Haushaltseinkommen_ln,
               Outcome=all_of(outcome_name)) %>%
        mutate(MEAN=mean(Outcome, na.rm=TRUE))

    mymodell <- lm(Outcome ~
                     I(Jahr*Jahr*MEAN) + I(Jahr*MEAN) + Arbeitslosigkeit + 
                     SchulabgaengerohneAbschluss_adj + Beschaeftigtenquote + Bruttoverdienst_ln + BeschaeftigtemitakadAbschluss_adj + BeschaeftigteohneAbschluss_adj + Einkommensteuer_ln + Haushaltseinkommen_ln ,
                   data=mydata, na.action="na.exclude")
    
    mydata %>%
        mutate(Imputed=predict(mymodell, newdata=.)) %>%
        mutate(Outcome=ifelse(is.finite(Outcome), Outcome, Imputed)) %>% 
        mutate(Outcome=ifelse(Outcome < 0, 0, Outcome)) %>%
        pull(Outcome)
}

Impdata.imputed <- Impdata.imputed %>%
    mutate(SchulabgaengermitHochschulreife_adj=my_ts_imputer2(.,"SchulabgaengermitHochschulreife_adj"),
           SchulabgaengermitHochschulreife    =my_ts_imputer2(.,"SchulabgaengermitHochschulreife"))

summary(Impdata.imputed$SchulabgaengermitHochschulreife_adj)

Impdata.imputed <- Impdata.imputed %>%
    filter(Gemeindekennziffer != 16063104)

## IV. Faktorenanalyse (Hauptkomponentenanalyse)
## inklusive Generierung der Faktorscores
TS_Arbeitswelt_adj <- Impdata.imputed %>%
    filter(Jahr > 1999) %>%
    select(Beschaeftigtenquote, Arbeitslosigkeit, Bruttoverdienst_ln)

TS_Einkommen_adj <- Impdata.imputed %>%
    filter(Jahr > 1999) %>%
    select(Einkommensteuer_ln, Haushaltseinkommen_ln, Schuldnerquote) 

TS_Bildung_adj <- Impdata.imputed %>%
    filter(Jahr > 1999) %>%
    select(BeschaeftigtemitakadAbschluss_adj,
           BeschaeftigteohneAbschluss_adj,
           SchulabgaengerohneAbschluss_adj)

TS_Arbeitswelt_adj.pca <- prcomp(TS_Arbeitswelt_adj, center=TRUE, scale.=TRUE,
                                 retx=TRUE)
plot(TS_Arbeitswelt_adj.pca)

TS_Arbeitswelt_adj.pca <- prcomp(TS_Arbeitswelt_adj, center=TRUE, scale.=TRUE,
                                 retx=TRUE, rank.=1)
TS_Arbeitswelt_adj.pca

TS_Einkommen_adj.pca <- prcomp(TS_Einkommen_adj, center=TRUE, scale.=TRUE,
                               retx=TRUE) 
plot(TS_Einkommen_adj.pca)

TS_Einkommen_adj.pca <- prcomp(TS_Einkommen_adj, center=TRUE, scale.=TRUE,
                               retx=TRUE, rank.=1) 
TS_Einkommen_adj.pca

TS_Bildung_adj.pca <- prcomp(TS_Bildung_adj, center=TRUE, scale.=TRUE,
                             retx=TRUE) 
plot(TS_Bildung_adj.pca)

TS_Bildung_adj.pca <- prcomp(TS_Bildung_adj, center=TRUE, scale.=TRUE,
                             retx=TRUE, rank.=1) 
TS_Bildung_adj.pca

GISD_Komponents1 <- data.frame(Variable=rownames(TS_Arbeitswelt_adj.pca$rotation),
                               Dimension="Arbeitswelt",
                               Anteil=unname(TS_Arbeitswelt_adj.pca$rotation^2),
                               Score=unname(TS_Arbeitswelt_adj.pca$rotation))

GISD_Komponents2 <- data.frame(Variable=rownames(TS_Einkommen_adj.pca$rotation),
                               Dimension="Einkommen",
                               Anteil=unname(TS_Einkommen_adj.pca$rotation^2),
                               Score=unname(TS_Einkommen_adj.pca$rotation))

GISD_Komponents3 <- data.frame(Variable=rownames(TS_Bildung_adj.pca$rotation),
                               Dimension="Bildung (adj.)",
                               Anteil=unname(TS_Bildung_adj.pca$rotation^2),
                               Score=unname(TS_Bildung_adj.pca$rotation))

GISD_Komponents <- rbind(GISD_Komponents1, GISD_Komponents2, GISD_Komponents3) %>%
    mutate(Proportion=round(Anteil*100, digits=1))

Resultdataset <- Impdata.imputed %>%
    mutate(TS_Arbeitswelt_adj=c(predict(TS_Arbeitswelt_adj.pca, newdata=.)),
           TS_Einkommen_adj  =c(predict(TS_Einkommen_adj.pca,   newdata=.)),
           TS_Bildung_adj    =c(predict(TS_Bildung_adj.pca,     newdata=.)))

Resultdataset %>%
    select(TS_Arbeitswelt_adj, TS_Einkommen_adj, TS_Bildung_adj) %>%
    summary()

descs <- Resultdataset %>%
    select(-Bevoelkerung) %>%
    pastecs::stat.desc()

Resultdataset %>%
    select(Arbeitslosigkeit,
           TS_Arbeitswelt_adj, TS_Einkommen_adj, TS_Bildung_adj) %>%
    cor(use="pairwise.complete.obs")

if(cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Bildung_adj, use="pairwise.complete.obs") < 0) {
    Resultdataset$TS_Bildung_adj <- -Resultdataset$TS_Bildung_adj
}

if(cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Arbeitswelt_adj, use="pairwise.complete.obs") < 0) {
  Resultdataset$TS_Arbeitswelt_adj <- -Resultdataset$TS_Arbeitswelt_adj
}

if(cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Einkommen_adj, use="pairwise.complete.obs") < 0) {
  Resultdataset$TS_Einkommen_adj <- -Resultdataset$TS_Einkommen_adj
}

Resultdataset %>%
    select(Arbeitslosigkeit,
           TS_Arbeitswelt_adj, TS_Einkommen_adj, TS_Bildung_adj) %>%
    cor(use="pairwise.complete.obs")

GISD_Komponents

Resultdataset <- Resultdataset %>%
    mutate(TS_Arbeitswelt_adj=(TS_Arbeitswelt_adj-min(TS_Arbeitswelt_adj))/(max(TS_Arbeitswelt_adj)-min(TS_Arbeitswelt_adj)),
           TS_Einkommen_adj  =(TS_Einkommen_adj  -min(TS_Einkommen_adj))  /(max(TS_Einkommen_adj)  -min(TS_Einkommen_adj)),
           TS_Bildung_adj    =(TS_Bildung_adj    -min(TS_Bildung_adj))    /(max(TS_Bildung_adj)    -min(TS_Bildung_adj)),
           GISD_Score=TS_Arbeitswelt_adj + TS_Einkommen_adj + TS_Bildung_adj) %>%
    group_by(Jahr) %>%
    mutate(GISD_Score=(GISD_Score-min(GISD_Score)) / (max(GISD_Score)-min(GISD_Score)),
           GISD_Score=round(GISD_Score, digits=5)) %>%
    ungroup()

Resultdataset %>%
    select(TS_Arbeitswelt_adj, TS_Einkommen_adj, TS_Bildung_adj,
           GISD_Score) %>%
    summary()

Resultdataset %>%
    select(TS_Arbeitswelt_adj, TS_Einkommen_adj, TS_Bildung_adj,
           GISD_Score) %>%
    str()

## V.  Datenexport - Erstellung der Datensätze 
RawResult <- Resultdataset %>%
    left_join(id_dataset, by=c("Gemeindekennziffer", "Bevoelkerung")) %>%
    rename(year         =Jahr, 
           population   =Bevoelkerung, 
           gisd_score   =GISD_Score, 
           gemeinde_id  =Gemeindekennziffer,
           gvb_id       =GVBKennziffer,
           kreis_id     =Kreiskennziffer,
           ror_id       =`Raumordnungsregion Nr`,
           nuts_2_id    =NUTS2,
           gemeinde_name=`Name der Gemeinde`,
           gvb_name     =`Name des Gemeindeverbands`,
           kreis_name   =`Name des Kreises`,
           ror_name     =Raumordnungsregion,
           nuts_2_name  =`NUTS2 Name`) %>%
  mutate(gemeinde_id=sprintf("%.8d", gemeinde_id),
         ## DW
         ## gvb_id hat zwischen 5 und 8 Zeichen, ist hier "<" gemeint?
         ## erzielt aber auch keine 8 Zeichen wenn < 7
         gvb_id     =sprintf("%.8d", as.numeric(gvb_id)),
         # gvb_id     =ifelse(nchar(gvb_id)      == 8, paste0("0", gvb_id),      gvb_id),
         kreis_id   =sprintf("%.5d", as.numeric(kreis_id)),
         ror_id     =sprintf("%.4d", as.numeric(ror_id)))

exportlist <- data.frame(Kennziffern=c("gemeinde_id",   "kreis_id",   "gvb_id",          "ror_id",             "nuts_2_id"),
                         Namen      =c("gemeinde_name", "kreis_name", "gvb_name",        "ror_name",           "nuts_2_name"),
                         Label      =c("Gemeinde",      "Kreis",      "Gemeindeverband", "Raumordnungsregion", "NUTS2"))

for(mykennziffer in exportlist$Kennziffern) {
    myname <- exportlist %>%
        filter(Kennziffern == mykennziffer) %>%
        pull(Namen)
  
    mylabel <- exportlist %>%
        filter(Kennziffern == mykennziffer) %>%
        pull(Label)
  
    print(paste("Level:", myname,"Label:", mylabel))

    outputdata <- RawResult 
  
    mergedataset <- outputdata %>%
        select(all_of(mykennziffer),
               all_of(myname),
               Bundesland) %>%
        group_by(!!rlang::sym(mykennziffer)) %>%
        slice_head(n=1) %>%
        ungroup()
  
    outputdata.agg <- outputdata %>%
        select(all_of(mykennziffer),
               year, population, gisd_score,
               TS_Bildung_adj, TS_Einkommen_adj, TS_Arbeitswelt_adj) %>%
        group_by(!!rlang::sym(mykennziffer), year) %>%
        summarise(gisd_score        =weighted.mean(gisd_score,         population),
                  TS_Bildung_adj    =weighted.mean(TS_Bildung_adj,     population),
                  TS_Einkommen_adj  =weighted.mean(TS_Einkommen_adj,   population),
                  TS_Arbeitswelt_adj=weighted.mean(TS_Arbeitswelt_adj, population),
                  population=sum(population)) %>%
        ungroup() %>%
        left_join(mergedataset, by=mykennziffer) %>%
        select(all_of(mykennziffer),
               all_of(myname),
               year, Bundesland, population, gisd_score,
               TS_Bildung_adj, TS_Einkommen_adj, TS_Arbeitswelt_adj) %>%
        as_tibble()
  
    outputdata.agg <- outputdata.agg %>%
        group_by(year) %>%
        mutate(gisd_score=(gisd_score-min(gisd_score))/(max(gisd_score)-min(gisd_score)),
               gisd_5 =findInterval(gisd_score, quantile(gisd_score, probs=(0:5)/5,   type=9)),
               gisd_5 =findInterval(gisd_5,  1:5),
               gisd_10=findInterval(gisd_score, quantile(gisd_score, probs=(0:10)/10, type=9)),
               gisd_10=findInterval(gisd_10, 1:10),
               gisd_k =findInterval(gisd_5,  c(1, 2, 5)),
               gisd_score=round(gisd_score, digits=5)) %>%
        ungroup()
  
    outputdata.agg %>%
        select(contains("gisd")) %>%
        summary()
    
    mydata <- outputdata.agg %>%
        select(all_of(mykennziffer),
               gisd_score, gisd_5, gisd_10, gisd_k,
               all_of(myname),
               year)
    
    write.csv(mydata,
              paste0(outfiles_dir, "2022_v03/Bund/GISD_Bund_", mylabel, ".csv"),
              row.names=FALSE, fileEncoding="UTF-8")
    
    names(mydata) <- gsub("\\.", "_",  make.names(names(mydata)))
    names(mydata) <- gsub("\\?", "oe", names(mydata))
    names(mydata) <- gsub("\\?", "ae", names(mydata))
    names(mydata) <- gsub("\\?", "ue", names(mydata))
    names(mydata) <- gsub("\\?", "ss", names(mydata))
    write_dta(mydata,
              paste0(outfiles_dir, "2022_v03/Bund/GISD_Bund_", mylabel, "_long.dta"))
    
    if(mylabel %in% c("Gemeindeverband", "Kreis")) {
        outputdata <- RawResult
        mergedataset <- outputdata %>%
            select(all_of(mykennziffer),
                   all_of(myname),
                   Bundesland) %>%
            group_by(!!sym(mykennziffer)) %>%
            slice_head(n=1) %>%
            ungroup()
        
        outputdata.bula <- outputdata %>%
            select(all_of(mykennziffer),
                   year, population, gisd_score,
                   TS_Bildung_adj, TS_Einkommen_adj, TS_Arbeitswelt_adj) %>%
            group_by(!!sym(mykennziffer), year) %>%
            summarise(gisd_score        =weighted.mean(gisd_score,         population),
                      TS_Bildung_adj    =weighted.mean(TS_Bildung_adj,     population),
                      TS_Einkommen_adj  =weighted.mean(TS_Einkommen_adj,   population),
                      TS_Arbeitswelt_adj=weighted.mean(TS_Arbeitswelt_adj, population),
                      population=sum(population))
    
        outputdata.bula <- outputdata.bula %>%
            left_join(mergedataset, by=mykennziffer) %>%
            select(all_of(mykennziffer),
                   all_of(myname),
                   year, Bundesland, population, gisd_score,
                   TS_Bildung_adj, TS_Einkommen_adj, TS_Arbeitswelt_adj) %>%
            as_tibble()
    
        outputdata.bula <- outputdata.bula %>%
            filter(!(Bundesland %in% c("Bremen", "Hamburg", "Berlin"))) %>%
            group_by(year, Bundesland) %>%
            mutate(gisd_score=(gisd_score-min(gisd_score))/(max(gisd_score)-min(gisd_score)),
                   gisd_score=round(gisd_score, digits=5)) %>%
            ungroup()

        # Ausgabe Bundesländer
        ListeBula <- unique(outputdata.bula$Bundesland)

        for(myland in ListeBula) {
            mydata.bula <- outputdata.bula %>%
                filter(Bundesland == myland) %>%
                select(gisd_score,
                       all_of(mykennziffer),
                       all_of(myname),
                       year)
            
            write.csv(mydata.bula,
                      paste0(outfiles_dir, "2022_v03/Bundesland/GISD_", myland, "_", mylabel, ".csv"),
                      row.names=FALSE, fileEncoding="UTF-8")
      
            names(mydata.bula) <- gsub("\\.", "_",  make.names(names(mydata.bula)))
            names(mydata.bula) <- gsub("\\?", "oe", names(mydata.bula))
            names(mydata.bula) <- gsub("\\?", "ae", names(mydata.bula))
            names(mydata.bula) <- gsub("\\?", "ue", names(mydata.bula))
            names(mydata.bula) <- gsub("\\?", "ss", names(mydata.bula))
            write_dta(mydata.bula,
                      paste0(outfiles_dir, "2022_v03/Bundesland/GISD_", myland,"_", mylabel, ".dta"))
        }
    }  
}

## VI.  Datensätze für PLZ generieren
## DW
## Datei nicht vorhanden
## Code ungetestet
load("Data/SHP/GEM_Zipcode_Intersections_2015.RData") # AGS/Postcode-Intersections-Dataset in sf format

for(mykennziffer in c("PLZ2", "PLZ3", "PLZ4", "PLZ5")) {
    myname  <- mykennziffer
    mylabel <- mykennziffer
    print(paste("Level:", myname, "Label:", mylabel))
    
    outputdata <- Resultdataset
    outputdata <- outputdata %>%
        rename(gemeinde_id=Gemeindekennziffer,
               year=Jahr,
               gisd_score=GISD_Score)
    
    outputdata <- outputdata %>%
        select(AGS=gemeinde_id, year, gisd_score)
    
    outputdata <- as.data.frame(PLZ.df) %>%
        ungroup() %>%
        mutate(AGS=as.numeric(as.character(AGS))) %>%
        left_join(outputdata, by="AGS", all.x=TRUE) %>%
        mutate(AGS=sprintf("%.8d", as.character(AGS)))
    
    outputdata <- outputdata %>%
        filter(!is.na(mykennziffer) & !is.na(EW_Area) & !is.na(year) & EW_Area > 0)
    
    mycol <- which(mykennziffer %in% names(outputdata))
    outputdata <- outputdata %>%
        group_by(year, AGS) %>%
        mutate(gisd_score=weighted.mean(gisd_score, EW_Area)) %>%
        ungroup() %>%
        group_by(year, !!rlang::sym(mykennziffer)) %>%
        summarise(gisd_score=weighted.mean(gisd_score, EW_Area),
                  population=sum(EW_Area)) %>%
        ungroup()
    
    outputdata <- outputdata %>%
        group_by(year) %>%
        mutate(gisd_score=(gisd_score-min(gisd_score))/(max(gisd_score )-min(gisd_score)),
               gisd_5 =findInterval(gisd_score, quantile(gisd_score, probs=0:5/5,   type=9)),
               gisd_5 =findInterval(gisd_5, 1:5),
               gisd_10=findInterval(gisd_score, quantile(gisd_score, probs=0:10/10, type=9)),
               gisd_10=findInterval(gisd_10, 1:10),
               gisd_k =findInterval(gisd_5, c(1, 2, 5)),
               ## DW
               ## GISD Score oben wird auf 5 Stellen gerundet
               gisd_score=round(gisd_score, digits=6)) %>%
        ungroup()
    
    summary(outputdata)            
    head(outputdata)
    
    ListeJahre <- unique(outputdata$year)
    write.csv(mydata,
              paste0(outfiles_dir, "2022_v03/Bund/GISD_Bund_", mylabel, ".csv"),
              row.names=FALSE, fileEncoding="UTF-8")
    
    mydata <- outputdata
    names(mydata) <- gsub("\\.", "_",  make.names(names(mydata)))
    names(mydata) <- gsub("\\?", "oe", names(mydata))
    names(mydata) <- gsub("\\?", "ae", names(mydata))
    names(mydata) <- gsub("\\?", "ue", names(mydata))
    names(mydata) <- gsub("\\?", "ss", names(mydata))
    write_dta(mydata,
              paste0(outfiles_dir, "2022_v03/Bund/GISD_Bund_", mylabel, "_long.dta"))
}
