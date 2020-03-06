if(!nzchar(system.file(package = "tidyverse")))
    install.packages("tidyverse")
if(!nzchar(system.file(package = "pdftools")))
    install.packages("pdftools")
if(!nzchar(system.file(package = "stringr")))
    install.packages("stringr")
if(!nzchar(system.file(package = "magrittr")))
    install.packages("magrittr")
library("tidyverse")
library("pdftools")
library("stringr")
library("magrittr")
                                    #Criação das pastas necessarias
desktop <- file.path(Sys.getenv("USERPROFILE"),"Desktop")
desktop <- str_replace(desktop,"/","\\\\")
setwd(desktop)
dir.create("PDF-CSV")
wd <- desktop %>% str_c("\\PDF-CSV")
setwd(wd)
dir.create("SICOOB")
setwd(wd)
wd <- desktop %>% str_c("\\PDF-CSV\\SICOOB")
setwd(wd)
dir.create("PDF")
dir.create("CSV")
dir.create("TXT")
old <- str_c(wd,"\\PDF")
setwd(old)
dir.create("OLD")
setwd(wd)