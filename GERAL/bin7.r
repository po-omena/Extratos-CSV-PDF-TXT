#Esse código extrai uma tabela de um arquivo PDF de extrato de banco SICOOB.
#Utilizando tecnicas de data wrangling no R para estruturar e posteriormente
#exportar os dados para .csv e .txt

#@author: Paulo Otavio Omena dos Santos
### SICOOB ANO UNICO
#date: 04/03/2020
#                   Inicio do código de processamentod e dados
print("Parsing the Data via Bank 7...")
tab <- str_split(text, "\n")
mx <- length(tab)
setwd(folder_path)
for (i in 1:mx) ################ LIMPANDO A TABELA
    {
        tab[[i]] <- (tab[[i]]%>%str_replace("\\.","")%>%str_replace("\r",""))
    }
pag_in <- 1          #DEFININDO PAGINA INICIAL
for (i in 1:length(tab[[pag_in]]))             #DEFININDO LINHA INICIAL
    {
        if(str_detect(tab[[pag_in]][i], "Valor"))
            lin_in <- i
    }
            #    LAÇO PARA REMOVER CABEÇALHO DE DADOS INÚTEIS DO CABEÇALHO
tab[[pag_in]][1:(lin_in - 1)] <- ""
tab[[pag_in]][lin_in] <- ( tab[[pag_in]][lin_in]                #LIMPANDO A LINHA INDICE
                           %>%str_replace("\\.","")
                           %>%str_replace("\r",""))
################################### DEFININDO PAGINA FINAL ###############################
pag_fim <- str_detect(tab, "A - Saldo") 
for (i in 1:mx)                 #DEFININDO PAGINA FINAL
    {
        if(isTRUE(pag_fim[i]))
            pag_fim <- i
    }
lin_fim <- (tab[[pag_fim]] %>% str_detect("A - Saldo")) #DEFININDO A LINHA FINAL#
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
tab2 <- tab2[!str_detect(tab2,"https")]
tab2 <- tab2[!str_detect(tab2,"Banking")]
tab2 <- tab2[tab2 != ""]
# ################## ESTRUTURANDO A NATUREZA DA OPERAÇÃO E REMOÇÃO DE SALDO DO DIA #############################
for (i in 1:length(tab2)) 
    {
        if(str_detect(tab2[i]," (D|C)"))
            {
                if(str_length(tab2[i])<3)
                    x <- i
                    if(x >0)
                        {
                            tab2[x-1] <- str_c(tab2[x-1],tab2[x])
                            tab2[x] <- ""
                            x <- 0
                        }
            }
        if(str_detect(tab2[i],"SALDO DO DIA" ) | str_detect(tab2[i],"SALDO" ))
            {
                tab2[i] <- ""
            }
    }
tab2[1] <- ""
tab2 <- tab2[tab2 != ""]
##################################### REMOÇÃO DE DADOS ADICIONAIS #######################
aux <- NULL
for (i in seq_along(tab2))
    {
        if(str_detect(tab2[i],"\\-?\\d?\\d?\\d?\\d?\\.?\\d?\\d?\\d?,\\d\\d"))
             aux <- c(aux,str_extract(tab2[i],"\\-?\\d?\\d?\\d?\\d?\\.?\\d?\\d?\\d?,\\d\\d"))   
    }
val <- NULL
val <- aux
VALOR <- val[!is.na(val)]
VALOR <- str_replace_all(VALOR,"\\.","")
NATUREZA <- NULL
for (i in 1:length(VALOR))
    {
        if(str_detect(VALOR[i],"\\-"))
            NATUREZA <- c(NATUREZA,"D")
        else NATUREZA <- c(NATUREZA,"C")
    }
######################## Separando Natureza da operação do valor ####################################
conta.deb <- NULL
conta.cred <- NULL
NATUREZA <- replace_na(NATUREZA,"X")
for (i in 1:length(NATUREZA))
    {
        if(NATUREZA[i] == "D")
            {
                conta.deb <- c(conta.deb,"2017")
                conta.cred <- c(conta.cred,"1014")
            }
        else if(NATUREZA[i] == "C")
            {
                conta.deb <- c(conta.deb,"1014")
                conta.cred <- c(conta.cred,"1022")
            }
        else if(NATUREZA[i] == "X")
            {
                conta.deb <- c(conta.deb,"4920")
                conta.cred <- c(conta.cred,"0")
            }
    }
######################## Adicionando as contas a serem creditadas e debitadas ##################
################################### REMOÇÃO DA DATA DA TABELA SECUNDÁRIA ########################
date <- NULL
for (i in 1:length(tab2))
    {
        date  <- c(date,str_extract(tab2[i],"\\d\\d/\\d\\d/\\d\\d\\d\\d"))
    }
DATA <- date[!is.na(date)]
tamD <- length(DATA)
rm(date)
######################## CONCATENAÇÃO DA LISTA SENDO A DATA CONSIDERADA O INDICE #####################
for (i in seq_along(tab2))
    {
        if(str_detect(tab2[i],"\\d\\d/\\d\\d/\\d\\d\\d\\d") & 
            str_detect(tab2[i],"\\-?\\d?\\d?\\d?\\d?\\.?\\d?\\d?\\d?,\\d\\d") &
            str_length(tab2[i]) < 40 )
            {
                if((!str_detect(tab2[i-1],"\\d\\d/\\d\\d/\\d\\d\\d\\d") & 
                    !str_detect(tab2[i-1],"\\-?\\d?\\d?\\d?\\d?\\.?\\d?\\d?\\d?,\\d\\d")))
                        {
                            tab2[i] <- str_c(tab2[i], tab2[i-1])
                            tab2[i-1] <- ""
                        }
                if((!str_detect(tab2[i+1],"\\d\\d/\\d\\d/\\d\\d\\d\\d") & 
                    !str_detect(tab2[i+1],"\\-?\\d?\\d?\\d?\\d?\\.?\\d?\\d?\\d?,\\d\\d")))
                        {
                            tab2[i] <- str_c(tab2[i], tab2[i+1])
                            tab2[i+1] <- ""
                        }
            }
    }
tab2 <- tab2[tab2!=""]
########################### REMOÇÃO DA DATA PARA DEFINIÇÃO DO HISTÓRICO #######################
if(length(tab2) > length(DATA))
    {
        tab2 <- tab2[str_detect(tab2,"\\-?\\d?\\d?\\d?\\d?\\.?\\d?\\d?\\d,\\d\\d")]
    }
tab2 <- str_replace_all(tab2,"\\d\\d/\\d\\d/\\d\\d\\d\\d ","")
tab2 <- tab2%>%str_replace_all("\\-?\\d?\\d?\\d?\\d?\\.?\\d?\\d?\\d,\\d\\d","")
HISTORICO <- tab2
HISTORICO <- str_replace_all(HISTORICO,"\\d\\d/\\d\\d/\\d\\d\\d?\\d?","")
HISTORICO <- str_trunc(HISTORICO,100, "right")
df <- data.frame(CONTA_DEB=conta.deb,CONTA_CRED=conta.cred,
                 VALOR=VALOR,DATA=DATA,HISTORICO=HISTORICO) 
fileName <- (filenames[z]%>%str_replace(".pdf",".csv")
                         %>%str_replace("PDF/","CSV/"))
fileName2 <- (filenames[z]%>%str_replace(".pdf",".txt")
                         %>%str_replace("PDF/","TXT/"))                                 
write.csv(df, file=fileName, row.names = FALSE)
write.table(df, fileName2, append = FALSE, sep = " ", dec = ",",row.names = FALSE, col.names = TRUE)

cat("Current file: ", filenamespure[z],"\n")
cat("Status: Finished.\n\n")