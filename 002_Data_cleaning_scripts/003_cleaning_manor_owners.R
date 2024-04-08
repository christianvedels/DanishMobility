rm(list=ls())

library(tidyverse)
library(stringr)
library(readxl)
library(openxlsx)

owners = read.csv("../Data/elite/manor_owners.csv")

# removing entities that we are not interested in (e.g. cities, dukes, companies, etc.). Ultimately, we only want individuals whose surname we can observe

owners = owners %>% filter(value != "Kronen" & 
                             value != "Staten" & 
                             value != "Statens Jordlovsudvalg" & 
                             value != "Statens Ungdomsskoleudvalg" &
                             value != "Konsortium" &
                             value != "Tønder amt" &
                             value != "Fyns Bispestol" &
                             value != "Den Danske Stat" &
                             value != "Den danske Stat" &
                             value != "Danske stat" &
                             value != "Den danske stat" &
                             value != "Statskassen" &
                             value != "Preussiske stat" &
                             value != "Flyvestation Avnø" &
                             value != "Statskassen" &
                             value != "Statsanstalten for Livsforsikring" &
                             value != "Direktionen for Statsgælden og den synkende Fond" &
                             value != "Julemærkekomiteen" &
                             value != "Tersløsekomite" &
                             value != "Odensebisperne" &
                             value != "Roskildebispen" &
                             value != "Sognepræsten ved Roskilde Domkirke og borgmesteren i Ringsted på vegne af forskellige kirker og skoler." &
                             value != "Nakskov Kirke" &
                             value != "Farsø Kirke" &
                             value != "Trinitatis kirke" &
                             value != "Mariager Kirke" &
                             value != "Sankt Knuds Kloster i Odense" &
                             value != "Skt. Knuds Kloster i Odense" &
                             value != "Sankt Clara Kloster i Roskilde" &
                             value != "Odense by" &
                             value != "" &
                             value != "Roskilde Sankt Clara Kloster" &
                             value != "Roskilde Domkapitel" &
                             value != "Roskilde Vor Frue Kloster" &
                             value != "Roskilde adelige Jomfrukloster" &
                             value != "Vor Frue Kloster i Roskilde" &
                             value != "Ribe Domkapitel" &
                             value != "Ribe kapitel" &
                             value != "Viborg Domkapitel" &
                             value != "Viborg domkapitel" &
                             value != "Viborg Domkirke" &
                             value != "Statens Jordlovsudsalg" &
                             value != "Jordlovsudvalget" &
                             value != "Ejerrække" &
                             value != "Region Syddanmark" &
                             value != "Preussiske Domæneforvaltning" &
                             value != "Fyens Disconto Kasse" &
                             value != "Horsens Sparekasse" &
                             value != "Den Almindelige Enkekasse" &
                             value != "Den kongelige kasse" &
                             value != "Sygekasserne i Skanderborg, Horsens, Kolding" &
                             value != "Centralkassen" &
                             value != "Krigshospitalskassen" &
                             value != "Aalborg Bys og Omegns Sparekasse" &
                             value != "Vitskøl Kloster" &
                             value != "Ringsted kloster" &
                             value != "Børglum Kloster" &
                             value != "Maribo Kloster" &
                             value != "Hundslund Kloster" &
                             value != "Øm Kloster" &
                             value != "Sorø Kloster" &
                             value != "Sankt Peders Kloster" &
                             value != "Halstedkloster" &
                             value != "Benediktinerkloster" &
                             value != "Sorø kloster" &
                             value != "Alling Kloster" &
                             value != "Ring Kloster" &
                             value != "Antvorskov Kloster" &
                             value != "Sankt Hans Kloster ejer gods i Gjesing, der muligvis er Gjesingholm" &
                             value != "Essenbæk Kloster ejer gods i Gjesing, der muligvis er Gjesingholm" &
                             value != "Tvilum Kloster" &
                             value != "Halsted Kloster" &
                             value != "Skt. Knuds Kloster" &
                             value != "Vemmetofte Kloster" &
                             value != "Vrejlev Kloster" &
                             value != "Dalum Kloster" &
                             value != "Tvis Kloster" &
                             value != "Ringsted Kloster" &
                             value != "Skt. Hans Kloster" &
                             value != "Klosterkammeret i Hannover" &
                             value != "Veng Kloster, Benediktinerordenen" &
                             value != "Interessantskab" &
                             value != "Indre Mission og KFUM og K" &
                             value != "Tvinds lærergruppe" &
                             value != "Parterne samles i begyndelsen af 1600-tallet:" &
                             value != "Erholm deltes i en broderpart og en søsterpart i 1507:" &
                             value != "Broderpart:" &
                             value != "Søsterpart:" &
                             value != "Wellendorfs umyndige børn" &
                             value != "Børnehjemmet Aaløkke" &
                             value != "Børneforsorgen" &
                             value != "Spædbørnshjemmet Nislevgaard" &
                             value != "Formodentlig Kronen" &
                             value != "Rentekammeret" &
                             value != "Mødrehjælpen" &
                             value != "Afholdsrestauration" &
                             value != "Skifteretten" &
                             value != "Kreditorer" &
                             value != "Bikuben" &
                             value != "Kosortium" &
                             value != "Dødsbo" &
                             value != "De holstenske grever" &
                             value != "Parterne samledes i 1651" &
                             value != "Nevøer og niecer til Henrik Heest" &
                             value != "Jacob Flebs børn" &
                             value != "Den ene part:" &
                             value != "Den anden part:" &
                             value != "Øster Søgaard" &
                             value != "Vester Søgaard" &
                             !grepl("kloster", value) &
                             !grepl("hospital", value) &
                             !grepl("Hospital", value) &
                             !grepl("Aalborg", value) &
                             !grepl("Mariager", value) &
                             !grepl("A/S", value) & 
                             !grepl("a/s", value) &
                             !grepl("forening", value) &
                             !grepl("Forening", value) &
                             !grepl("Foreningen", value) &
                             !grepl("Konsortium", value) &
                             !grepl("konsortium", value) &
                             !grepl("ApS", value) &
                             !grepl("Aps", value) &
                             !grepl("banken", value) &
                             !grepl("bank", value) &
                             !grepl("Bank", value) & 
                             !grepl("museum", value) &
                             !grepl("Museum", value) &
                             !grepl("Amt", value) &
                             !grepl("jylland", value) &
                             !grepl("Kriminalforsorgen", value) &
                             !grepl("Fond", value) &
                             !grepl("fonden", value) &
                             !grepl("Fonden", value) &
                             !grepl("fond", value) &
                             !grepl("kommune", value) &
                             !grepl("Kommune", value) &
                             !grepl("fideikommis", value) &
                             !grepl("Fideikommis", value) &
                             !grepl("Kommissionen", value) &
                             !grepl("bispestol", value) &
                             !grepl("Bispestol", value) &
                             !grepl("Stift", value) &
                             !grepl("stift", value) &
                             !grepl("København", value) &
                             !grepl("Aarhus", value) &
                             !grepl("Landbrug", value) &
                             !grepl("skole", value) &
                             !grepl("Skole", value) &
                             !grepl("ejere", value) &
                             !grepl("Ejere", value) &
                             !grepl("Ukendt", value) &
                             !grepl("institution", value) &
                             !grepl("Institution", value) &
                             !grepl("Realkredit", value) &
                             !grepl("Interessentskab", value) &
                             !grepl("Interessentselskab", value) &
                             !grepl("selskab", value) &
                             !grepl("Pension", value) &
                             !grepl("Jomfrukloster", value) &
                             !grepl("Nonnekloster", value) &
                             !grepl("ordenen", value) &
                             !grepl("I/S", value) &
                             !grepl("Grevskab", value) &
                             !grepl("Kongefamilien", value) &
                             !grepl("Familielegat", value) &
                             !grepl("De Spanjerske Legater", value) &
                             !grepl("fabrik", value) &
                             !grepl("Handelshus", value) &
                             !grepl("Stamhuset", value) &
                             !grepl("Akademi", value) &
                             !grepl("styrelsen", value) &
                             !grepl("ministeriet", value) &
                             !grepl("museet", value) &
                             !grepl("Museet", value) &
                             !grepl("Konkusbo", value) &
                             !grepl("Konkursbo", value) &
                             !grepl("Bolig", value) &
                             !grepl("Skødet", value) &
                             !grepl("Bønderne", value) &
                             !grepl("bønderne", value) &
                             !grepl("brødre", value) &
                             !grepl("Aktivitetscentret", value) &
                             !grepl("Fallitbo", value) & 
                             !grepl("Klædefirmaet", value) &
                             !grepl("parthavere", value) &
                             !grepl("svigerinde", value) &
                             !grepl("niecer", value) &
                             !grepl("Vallø", value) &
                             !grepl("Hesbjerg", value) &
                             !grepl("Menighedsråd", value) &
                             !grepl("Datter", value) &
                             !grepl(" af ", value) &
                             !grepl("arvinger", value) &
                             !grepl("Arvinger", value) &
                             !grepl("dødsbo", value) &
                             !grepl("Dødsbo", value) &
                             !grepl("Dronning", value) &
                             !grepl("prins", value) &
                             !grepl("Prins", value) &
                             !grepl("Efterkommere", value) &
                             !grepl("Boet efter", value) &
                             !grepl("søstre", value) &
                             !grepl("K/S", value) &
                             !grepl("enke efter", value) &
                             !grepl("Enke efter", value) &
                             !grepl("Godsejer", value) &
                             !grepl("Firmaet", value) &
                             !grepl("& Co.", value) &
                             !grepl("Sognekald", value) &
                             !grepl("herefter", value) &
                             !grepl("optagelseshjem", value) &
                             !grepl("kreditorer", value) &
                             !grepl("Døtre", value) &
                             !grepl("døtre", value) &
                             !grepl("Kreditorer", value)) %>% 
  mutate(value = str_remove(value, " og hustru"),
         value = str_remove(value, " d.yngre."),
         value = str_remove(value, " d. yngre."),
         value = str_remove(value, " d. yngre"),
         value = str_remove(value, " den yngre"),
         value = str_remove(value, " den Yngre"),
         value = str_remove(value, ", den ældre"),
         value = str_remove(value, " den ældre"),
         value = str_remove(value, " d. ældre."),
         value = str_remove(value, " d.æ."),
         value = str_remove(value, " d.y."),
         value = str_remove(value, " samt søskende."),
         value = str_remove(value, " & søn"),
         value = str_remove(value, " i Roskilde"),
         value = str_remove(value, " m.fl."))

owners$personID = seq(1:nrow(owners))

# removing part of name where it states the married name (which is saved in marriageSurname)

owners = owners %>% mutate(value = str_split(value, "\\, gift", simplify = T)[,1],
                           value = str_split(value, "\\,gift", simplify = T)[,1],
                           value = str_split(value, "\\.gift", simplify = T)[,1],
                           value = str_split(value, "\\. gift", simplify = T)[,1],
                           value = str_split(value, "\\gift", simplify = T)[,1],
                           value = str_split(value, "\\, g.", simplify = T)[,1])

# function that splits a string by either " og ", "," or "/"

split_based_on_content <- function(input_string) {
  if (grepl(",|/||& og ", input_string)) {
    return(unlist(strsplit(input_string, ",|/|&| og ")))
  } else {
    return(input_string)
  }
}

split_strings <- lapply(owners$value, split_based_on_content)

max_parts <- max(sapply(split_strings, length))

split_strings <- lapply(split_strings, function(x) c(x, rep(NA, max_parts - length(x))))

df <- as.data.frame(do.call(rbind, split_strings))

colnames(df) <- c("owner1", "owner2", "owner3", "owner4")

owners = cbind(owners, df)

remove(df, split_strings, split_based_on_content, max_parts) # removing temporary items

owners = owners %>% mutate(owner1 = trimws(owner1),
                           owner2 = trimws(owner2),
                           owner3 = trimws(owner3),
                           owner4 = trimws(owner4), # removes redundant spaces
                           surname1 = word(owner1, -1),
                           surname2 = word(owner2, -1),
                           surname3 = word(owner3, -1),
                           surname4 = word(owner4, -1)) # takes the string after the last space in the name variable


owners = owners %>% mutate(yearTo = ifelse(str_detect(owners$years, "-") == F, yearFrom, yearTo))

owners = owners %>% mutate(yearFrom = str_remove(owners$yearFrom, "ø"),
                           yearTo = str_remove(owners$yearTo, "-"),
                           yearTo = str_remove(owners$yearTo, "ø"))

owners$yearFrom = trimws(owners$yearFrom)
owners$yearTo = trimws(owners$yearTo)

owners = owners %>% filter(surname1 != "I" & surname1 != "II" & surname1 != "III" & surname1 != "IV" & surname1 != "V" & surname1 != "VI" & surname1 != "VII" & surname1 != "VIII" & surname1 != "IX" & surname1 != "X")

owners = owners %>% mutate(surname1 = ifelse(surname1 == "EriksenLøvenbalk", "Løvenbalk", surname1))

owners = owners %>% mutate(yearFrom = str_replace_all(yearFrom, "-", ""),
                         yearFrom = str_split_fixed(yearFrom, ",", 2)[,1],
                         yearFrom = str_trim(yearFrom),
                         yearTo = str_replace_all(yearTo, "\\´", ""),
                         yearTo = str_replace_all(yearTo, "-", ""),
                         yearTo = str_split_fixed(yearTo, ",", 2)[,1],
                         yearTo = str_trim(yearTo))

owners[owners==""] = NA

owners = owners %>% mutate(yearFrom = as.numeric(yearFrom),
                           yearTo = as.numeric(yearTo))

#write.xlsx(owners, "Documents/Research/Surnames/data/manor_owners_cleaned.xlsx", rowNames = F)
write.csv(owners, "../Data/elite/manor_owners_cleaned.csv", row.names = F)

