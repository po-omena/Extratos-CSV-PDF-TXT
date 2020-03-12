#Esse código extrai uma tabela de um arquivo PDF de extrato de banco SICOOB.
#Utilizando tecnicas de data wrangling no R para estruturar e posteriormente
#exportar os dados para .csv e .txt

#@author: Paulo Otavio Omena dos Santos

#date: 04/03/2020

#Instalar e importar as bibliotecas necessarias
if(!nzchar(system.file(package = "tidyverse")))
    install.packages("tidyverse")
if(!nzchar(system.file(package = "pdftools")))
    install.packages("pdftools")
if(!nzchar(system.file(package = "stringr")))
    install.packages("stringr")
if(!nzchar(system.file(package = "magrittr")))
    install.packages("magrittr")
if(!nzchar(system.file(package = "tesseract")))
    install.packages("tesseract")
library("tidyverse")
library("pdftools")
library("stringr")
library("magrittr")
library("utils")
tesseract_download("por")
library("tesseract")
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
old <- str_c(wd,"\\SICOOB\\PDF\\OLD")
#                   Definição do caminho para os arquivos
folder_path <- wd
folder_path <- (folder_path%>%str_replace_all("/","\\\\")
                           %>%str_c("\\PDF"))
filenames <- list.files(folder_path, pattern="*.pdf", full.names=TRUE)
files <- list.files(folder_path, pattern="*.pdf", full.names=FALSE)
pdf_file <- filenames[1]
#                   Inicio do código de processamento de dados

ano <- readline(prompt=(paste("Entre com o Ano dos extratos: ")))
por <- tesseract("por")
for(z in 1:length(filenames))
    {
        pdf_file <- filenames[z]
        # text <- pdf_text(pdf_file)
        text <- tesseract::ocr(pdf_file,engine = por)
        tab <- str_split(text, "\n")
        mx <- length(tab)

        for (i in 1:mx) ################ LIMPANDO A TABELA
            {
                tab[[i]] <- (tab[[i]]%>%str_replace("\\.","")%>%str_replace("\r",""))
            }

        pag_in <- str_detect(tab,"DATA")          #DEFININDO PAGINA INICIAL
        for (i in 1:mx)                             #DEFININDO PAGINA INICIAL
            {
                if(isTRUE(pag_in[i]))
                    pag_in <- i
            }

        lin_in <- str_detect(tab[[pag_in]], "DATA") #DEFININDO LINHA INICIAL
        for (i in 1:length(tab[[pag_in]]))             #DEFININDO LINHA INICIAL
            {
                if(isTRUE(lin_in[i]))
                  lin_in <- i
            }

                       #LAÇO PARA REMOVER CABEÇALHO DE DADOS INÚTEIS DO CABEÇALHO
        tab[[pag_in]][1:(lin_in - 1)] <- ""
        tab[[pag_in]][lin_in] <- ( tab[[pag_in]][lin_in]                #LIMPANDO A LINHA INDICE
                                   %>%str_replace("\\.","")
                                   %>%str_replace("\r",""))

        ################################### DEFININDO PAGINA FINAL ##################################################
        pag_fim <- str_detect(tab, "RESUMO") 
        for (i in 1:mx)                 #DEFININDO PAGINA FINAL
            {
                if(isTRUE(pag_fim[i]))
                    pag_fim <- i
            }

        lin_fim <- (tab[[pag_fim]] %>% str_detect("RESUMO")) #DEFININDO A LINHA FINAL#
        for (i in 1:length(tab[[pag_fim]]))         #DEFININDO A LINHA FINAL#
            {
                if(isTRUE(lin_fim[i]))
                    lin_fim <- i
            }
        ####################EXCLUINDO DADOS INUTEIS DO FINAL DO DOCUMENTO#################################
        tab[[pag_fim]][lin_fim:length(tab[[pag_fim]])] <- ""
        if(mx > pag_fim)
            {
                for (i in (pag_fim + 1):mx)
                    tab[[i]] <- ""
            }
        ##################### ESTRUTURANDO OS VALORES ##################################
        tab2 <- unlist(tab)
        tab2 <- str_replace_all(tab2,"\\s+"," ")
        x <- 0
        tab2 <- tab2[tab2 != ""]
        ################## REMOÇÃO DE SALDO DO DIA #############################
        for (i in 1:length(tab2)) 
            {
                if(str_detect(tab2[i],"SALDO DO DIA" ) |
                   str_detect(tab2[i],"SALDO" ) |
                   str_detect(tab2[i],"DEPCHEQUE BLOQ"))
                    {
                        tab2[i] <- ""
                    }
            }
        tab2[1] <- ""
        tab2 <- tab2[tab2 != ""]
        dat <- tab2[str_detect(tab2,"\\d\\d/\\d\\d/\\d\\d\\d\\d")]
        dat <- dat[!str_detect(dat,"about:blank")]
        dat[!str_detect(dat,"\\d?\\d,\\d\\d")] <- str_c(dat[!str_detect(dat,"\\d?\\d,\\d\\d")]," 0,00")
        tab2 <- dat
        ################################## REMOÇÃO DO VALOR DA TABELA SECUNDARIA ###########################
        val <- NULL
        val <- str_extract_all(tab2[1:length(tab2)],"\\d?\\d?\\d?\\d?\\.?\\d?\\d?\\d?,\\d\\d(D| D|C| C)?")
        VALOR <- val[!is.na(val)]
        VALOR <- VALOR[str_detect(VALOR,",\\d\\d")]
        aux <- NULL
        for (i in 1:length(VALOR))
            {
                if(!is.na(VALOR[[i]][2]))
                    aux <- c(aux,VALOR[[i]][2])
                else aux <- c(aux, VALOR[[i]][1])
            }
        VALOR <- aux
        VALOR <- str_replace_all(VALOR,"\\.","")
        # rm(val)
        tab2 <- tab2%>%str_replace("\\d?\\d?\\d?\\d?\\.?\\d?\\d?\\d?,\\d\\d(D| D|C| C)?","")
        ######################### Separando Natureza da operação do valor ####################################
        NATUREZA <- str_extract(VALOR,"C|D")
        VALOR <- str_replace_all(VALOR,"C|D","")
        conta.deb <- NULL
        conta.cred <- NULL
        NATUREZA <- replace_na(NATUREZA,"X")
        for (i in 1:length(NATUREZA))
            {
                if(NATUREZA[i] == "D")
                    {
                        conta.deb <- c(conta.deb,"2017")
                        conta.cred <- c(conta.cred,"3520")
                    }
                else if(NATUREZA[i] == "C")
                    {
                        conta.deb <- c(conta.deb,"3520")
                        conta.cred <- c(conta.cred,"1022")
                    }
                else if(NATUREZA[i] == "X")
                    {
                        conta.deb <- c(conta.deb,"4920")
                        conta.cred <- c(conta.cred,"0")
                    }
            }
        ################################### REMOÇÃO DA DATA DA TABELA SECUNDÁRIA ########################
        tab2[str_detect(tab2,"Page \\d")] <- ""
        tab2[str_detect(tab2,"about:blank")] <- ""
        tab2 <- tab2[tab2 != ""]
        date <- NULL
        for (i in 1:length(tab2))
            {
                date  <- c(date,str_extract(tab2[i],"\\d\\d/\\d\\d/\\d\\d\\d\\d"))
            }
        DATA <- date[!is.na(date)]
        tamD <- length(DATA)
        rm(date)
        ######################## CONCATENAÇÃO DA LISTA SENDO A DATA CONSIDERADA O INDICE #####################
        in_a <- 0
        in_b <- 0
        tam <- length(tab2)
        tab2[tam+1] <- "FINALDATABELA"
        tam <- length(tab2)
        for (i in 1:tam)
            {
                if(str_detect(tab2[i],"\\d\\d/\\d\\d") | str_detect(tab2[i],"FINALDATABELA"))
                    {
                        if(in_a == 0)
                            in_a <- i
                        else if(in_b == 0)
                            in_b <- i - 1
                    }
                if(in_a == in_b & in_b != 0)
                    {
                        in_a <- i
                        in_b <- 0
                        next
                    }
               if(in_a > 0 & in_b > 0)
                   {
                       for (x in (in_a+1):in_b)
                           {
                               tab2[in_a] <- str_c(tab2[in_a],tab2[x])
                               tab2[x] <- ""
                           }
                       in_a <- i
                       in_b <- 0
                   }
            }
        tab2[tam] <- ""
        tab2 <- tab2[tab2!=""]
        tam <- length(tab2)
        ########################### REMOÇÃO DA DATA PARA DEFINIÇÃO DO HISTÓRICO #######################
        tab2 <- str_replace_all(tab2,"\\d\\d/\\d\\d/\\d\\d\\d\\d ","")
        tab2 <- str_replace_all(tab2,"\\d?\\d?\\d?\\d?\\.?\\d?\\d?\\d?,\\d\\d(D| D|C| C)?","")
        HISTORICO <- tab2
        df <- data.frame(CONTA_DEB=conta.deb,CONTA_CRED=conta.cred,
                         VALOR=VALOR,DATA=DATA,HISTORICO=HISTORICO) 
        fileName <- (filenames[z]%>%str_replace(".pdf",".csv")
                                 %>%str_replace("PDF/","CSV/"))
        fileName2 <- (filenames[z]%>%str_replace(".pdf",".txt")
                                 %>%str_replace("PDF/","TXT/"))                                 
        write.csv(df, file=fileName, row.names = FALSE)
        write.table(df, fileName2, append = FALSE, sep = " ", dec = ",",
            row.names = FALSE, col.names = TRUE)

    }
dest <- (filenames[1]%>%str_replace("PDF/","PDF\\\\OLD/")
                     %>%str_split("/"))
dest <- dest[[1]][1]
file.copy(filenames, dest)
file.remove(filenames)
print("Todos os arquivos foram convertidos.")


#CÓDIGO FINALIZADO POR HORA