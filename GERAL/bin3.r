#Esse código extrai uma tabela de um arquivo PDF de extrato de banco BRADESCO.
#Utilizando tecnicas de data wrangling no R para estruturar e posteriormente
#exportar os dados para .csv e .txt

#@author: Paulo Otavio Omena dos Santos
## Bradesco Limpo
#date: 04/03/2020

#                   Inicio do código de processamentod e dados
print("Parsing the Data via Bank 3...")
ano <- readline(prompt=(paste("Entre com o Ano do extrato ", filenamespure[z],":")))
pdf_file <- filenames[z]
text <- pdf_text(pdf_file)
tab <- str_split(text, "\n")
mx <- length(tab)

for (i in 1:mx) ################ LIMPANDO A TABELA
    {
        tab[[i]] <- (tab[[i]]%>%str_replace("\\.","")%>%str_replace("\r",""))
    }

pag_in <- 1          #DEFININDO PAGINA INICIAL
for (i in 1:length(tab[[pag_in]]))             #DEFININDO LINHA INICIAL
    {
        if(str_detect(tab[[pag_in]][i], "DATA"))
            lin_in <- i
    }

               #LAÇO PARA REMOVER CABEÇALHO DE DADOS INÚTEIS DO CABEÇALHO
tab[[pag_in]][1:(lin_in - 1)] <- ""
tab[[pag_in]][lin_in] <- ( tab[[pag_in]][lin_in]                #LIMPANDO A LINHA INDICE
                           %>%str_replace("\\.","")
                           %>%str_replace("\r",""))
################################### DEFININDO PAGINA FINAL ##################################################
for (i in 1:mx)
    {
        for (x in 1:length(tab[[i]]))
            {
                if(str_detect(tab[[i]][x],"TOTAL DA MOVIM"))
                    lin_fim <- x
                    pag_fim <- i
            }
    }
####################EXCLUINDO DADOS INUTEIS DO FINAL DO DOCUMENTO#################################
tab[[pag_fim]][lin_fim:length(tab[[pag_fim]])] <- ""
if(mx > pag_fim)
    {
        for (i in (pag_fim + 1):mx)
            tab[[i]] <- ""
    }
##################### ESTRUTURANDO A TABELA ##################################
tab2 <- unlist(tab)
################## ESTRUTURANDO A NATUREZA DA OPERAÇÃO E REMOÇÃO DE SALDO DO DIA #############################
for (i in (lin_in + 1):length(tab2)) 
    {
        if(str_detect(tab2[i],"SALDO" ))
            {
                tab2[i] <- ""
            }
    }
tab2 <- tab2[tab2 != ""]
tam <- length(tab2)
st <- str_pad(tab2,str_length(tab2[1]),"right")
st <- str_trunc(st,96,"right")
cred <- str_trunc(st,52,"left")
cred <- str_trunc(cred,36,"right")
aux <- NULL
NATUREZA <- NULL
for (i in 1:tam)
    {
        if(str_detect(cred[i],"\\d?\\d?\\d?\\d?\\.?\\d?\\d?\\d?,\\d\\d"))
            {
            aux <- c(aux,str_extract(cred[i],"\\d?\\d?\\d?\\d?\\.?\\d?\\d?\\d?,\\d\\d"))
            NATUREZA <- c(NATUREZA,"C")
            }
        else 
            {
                aux <- c(aux,str_extract(st[i],"\\d?\\d?\\d?\\d?\\.?\\d?\\d?\\d?,\\d\\d"))
                if(str_detect(tab2[i],"\\d?\\d?\\d?\\d?\\.?\\d?\\d?\\d?,\\d\\d"))
                   {
                        NATUREZA <- c(NATUREZA, "D")
                   }
            }
    }
#rm(cred,st,aux)
################################## REMOÇÃO DO VALOR DA TABELA SECUNDARIA ###########################
val <- NULL
val <- str_extract(tab2[1:length(tab2)],"\\d?\\d?\\d?\\d?\\.?\\d?\\d?\\d?,\\d\\d")
VALOR <- val[!is.na(val)]
VALOR <- str_replace_all(VALOR,"\\.","")
rm(val)
tab2 <- tab2%>%str_replace_all("\\d?\\d?\\d?\\d?\\.?\\d?\\d?\\d?,\\d\\d","")
######################### Separando Natureza da operação do valor e limpeza ####################################
tam <- length(tab2)
for (i in 1:tam)
    {
        if(str_detect(tab2[i],"Valor Disponivel"))
            tab2[i] <- ""
    }
tab2 <- tab2[tab2 != ""]
######################## CONCATENAÇÃO DA LISTA SENDO O DOCTO CONSIDERADO O INDICE #####################
in_a <- 0
in_b <- 0
tam <- length(tab2)
tab2[tam+1] <- "FINALDATABELA"
tam <- length(tab2)
for (i in 1:tam)
    {
        if(str_detect(tab2[i],"\\d?\\d?\\d?\\d?\\d?\\d?\\d?\\d?\\d?\\d   ") | 
           str_detect(tab2[i],"FINALDATABELA"))
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
                       tab2[in_a] <- str_c(tab2[in_a],"DOC",tab2[x])
                       tab2[x] <- ""
                   }
               in_a <- i
               in_b <- 0
           }
    }
tab2[tam] <- ""
tab2[1] <- ""
tab2 <- tab2[tab2!=""]
tam <- length(tab2)
tab2 <- (tab2%>%str_replace_all("\\s+"," ")%>%str_replace_all(" DOC","-DOC"))
################################ ADICIONANDO DATA PARA TODOS OS VALORES #############################
DATA <- NULL
in_a <- 0
in_b <- 0
tam <- length(tab2)
tab2[tam+1] <- "FINALDATABELA"
tam <- length(tab2)
for (i in 1:tam)
    {
        if(str_detect(tab2[i],"\\d\\d/\\d\\d/\\d\\d") | str_detect(tab2[i],"FINALDATABELA"))
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
               data <- str_extract(tab2[in_a],"\\d\\d/\\d\\d/\\d\\d")
               for (x in 1:length(tab2[in_a:in_b]))
                   {
                       DATA <- c(DATA,data)
                   }
               in_a <- i
               in_b <- 0
           }
    }
data <- str_split(DATA, "/")
tamD <- length(DATA)
for (i in 1:tamD)
    {
            data[[i]][1] <- str_c(data[[i]][1],"/",data[[i]][2],"/",ano)
            DATA[i] <- data[[i]][1]
    }
DATA <- DATA[DATA!=""]
rm(in_a,in_b)
tab2 <- (tab2%>%str_replace_all("\\d\\d/\\d\\d/\\d\\d"," ")
             %>%str_replace_all("\\s+"," ")%>%str_replace_all("FINALDATABELA",""))
tab2 <- tab2[tab2 != ""]
########################### REMOÇÃO DA DATA PARA DEFINIÇÃO DO HISTÓRICO #######################
conta.deb <- NULL
conta.cred <- NULL
NATUREZA <- replace_na(NATUREZA,"X")
for (i in 1:length(NATUREZA))
    {
        if(NATUREZA[i] == "D")
            {
                conta.deb <- c(conta.deb,"2017")
                conta.cred <- c(conta.cred,"3631")
            }
        else if(NATUREZA[i] == "C")
            {
                conta.deb <- c(conta.deb,"3631")
                conta.cred <- c(conta.cred,"1022")
            }
        else if(NATUREZA[i] == "X")
            {
                conta.deb <- c(conta.deb,"4920")
                conta.cred <- c(conta.cred,"0")
            }
    }
#################################### Define o HISTORICO #################
HISTORICO <- tab2
HISTORICO <- str_replace_all(HISTORICO,"\\d\\d/\\d\\d/\\d\\d\\d?\\d?","")
HISTORICO <- str_trunc(HISTORICO,100, "right")
######################## Atualizações adicionais #############################
NATUREZA[str_detect(HISTORICO,"TARIFA")] <- "D"
conta.deb[str_detect(HISTORICO,"TARIFA")] <- 3128
conta.deb[str_detect(HISTORICO,"APLICINVEST")] <- 3631
conta.cred[str_detect(HISTORICO,"APLICINVEST")] <- 3515
conta.deb[str_detect(HISTORICO,"RESGATE INVEST")] <- 3515
conta.cred[str_detect(HISTORICO,"RESGATE INVEST")] <- 3631
####################################################################################################
df <- data.frame(CONTA_DEB=conta.deb,CONTA_CRED=conta.cred,
                 VALOR=VALOR,DATA=DATA,HISTORICO=HISTORICO) 
fileName <- (filenames[z]%>%str_replace(".pdf",".csv")
                         %>%str_replace("PDF/","CSV/"))
fileName2 <- (filenames[z]%>%str_replace(".pdf",".txt")
                         %>%str_replace("PDF/","TXT/"))                                 
write.csv(df, file=fileName, row.names = FALSE)
write.table(df, fileName2, append = FALSE, sep = " ", dec = ",",
    row.names = FALSE, col.names = TRUE)

cat("Current file: ", filenamespure[z],"\n")
cat("Status: Finished.\n\n")
# TESTANDO A ANALISE DE TEXTO 2
