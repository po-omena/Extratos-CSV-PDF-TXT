origin <- getwd()
print("Checking Directories...")
desktop <- file.path(Sys.getenv("USERPROFILE"),"Desktop")
desktop <- str_replace(desktop,"/","\\\\")
setwd(desktop)
if (!file.exists("PDF-CSV"))
    dir.create("PDF-CSV")
wd <- desktop %>% str_c("\\PDF-CSV")
setwd(wd)
if (!file.exists("GERAL"))
    dir.create("GERAL")
setwd(wd)
wd <- desktop %>% str_c("\\PDF-CSV\\GERAL")
setwd(wd)

if (!file.exists("PDF"))
    dir.create("PDF")

if (!file.exists("CSV"))
    dir.create("CSV")

if (!file.exists("TXT"))
    dir.create("TXT")    
old <- str_c(wd,"\\PDF")

setwd(old)

if (!file.exists("OLD"))
    dir.create("OLD")

setwd(wd)
old <- str_c(wd,"\\GERAL\\PDF\\OLD")
folder_path <- wd
folder_path <- (folder_path%>%str_replace_all("/","\\\\")
                       %>%str_c("\\PDF"))
print("Loading Files...")
filenames <- list.files(folder_path, pattern="*.pdf", full.names=TRUE)
filenamespure <- list.files(folder_path, pattern="*.pdf", full.names=FALSE)
files <- length(filenames)
    for (z in 1:files)
        {
            pdf_file <- filenames[z]
            text <- pdf_text(pdf_file)
            teste_text <- unlist(text)
            if(str_detect(teste_text[[1]],"SICOOB"))
                {
					if(str_detect(teste_text[[1]],"8.430-1 - TOPP INOX") | 
					   str_detect(teste_text[[1]],"8.652-5 - HWO"))
						{
							setwd(origin)
							source("bin7.r")
						}
					else
						{
							setwd(origin)
							source("bin1.R")					
						}
                }
            else if(str_detect(teste_text[[1]],"C.AIXA") | str_detect(teste_text[[1]],"AIXA\r\n"))
                    {
                        setwd(origin)
                        source("bin2.R")
                    }
            else if(str_detect(teste_text[[1]], "DEMONSTRATIVO MENSAL - CONTA CORRENTE\r\nDATA"))
                {
                    setwd(origin)
                    source("bin3.r")
                }
            else if(str_detect(teste_text[[1]],"Agência | Conta\r\n"))
                {
                    setwd(origin)
                    source("bin4.r")
                }
            else if(str_detect(teste_text[[1]],"Dt. balancete Dt. movimento"))
                {
                    setwd(origin)
                    source("bin5.r")
                }            
            else
                {
                    cat("Banco não identificado.\nDigite manualmente o Nº do banco:\n1 - SICOOB\n2 - CAIXA\n3 - BRADESCO")
                    filenames[z] <- ""
                    next
                    # print(text)
                }
        }

print("Finishing...")
filesnames <- NULL
filesnames <- filesnames[filesnames != ""]
dest <- (filenames[z]%>%str_replace("PDF/","PDF\\\\OLD/")
                     %>%str_split("/"))
dest <- dest[[1]][1]
file.copy(filenames, dest)
file.remove(filenames)
cat("Todos os arquivos foram convertidos.\n\n\n")

<<<<<<< HEAD
=======

>>>>>>> master
