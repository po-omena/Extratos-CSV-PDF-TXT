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
files <- length(filenames)
    for (z in 1:files)
        {
            pdf_file <- filenames[z]
            text <- pdf_text(pdf_file)
            teste_text <- unlist(text)
            if(str_detect(teste_text[[1]],"SICOOB"))
                {
                    setwd(origin)
                    source("bin1.R")
                }
            else if(str_detect(teste_text[[1]],"C.AIXA") | str_detect(teste_text[[1]],"AIXA\r\n"))
                    {
                        setwd(origin)
                        source("bin2.R")
                    }
            else
                {
                    print("Não foi possivel identificar o Banco.")
                    filenames[z] <- ""
                    next
                }
        }

print("Finishing...")
filesnames <- filesnames[filesnames != ""]
dest <- (filenames[z]%>%str_replace("PDF/","PDF\\\\OLD/")
                     %>%str_split("/"))
dest <- dest[[1]][1]
file.copy(filenames, dest)
file.remove(filenames)
print("Todos os arquivos foram convertidos.")