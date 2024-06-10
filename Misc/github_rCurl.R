https://raw.githubusercontent.com/:HeatherWelch/:OPC_Farallons_end_products_final/master/:epac_metadata

https://api.github.com/repos/{username}/{repository_name}/contents/{file_path}

https://api.github.com/repos/HeatherWelch/OPC_Farallons_end_products_final/contents/blob/main/epac_metadata/2023/04/metadata_2023-04-01.csv
https://api.github.com/repos/HeatherWelch/OPC_Farallons_end_products_final/contents/epac_metadata/2023/04/metadata_2023-04-01.csv

https://raw.githubusercontent.com/HeatherWelch/repository/branch/filename


https://raw.githubusercontent.com/HeatherWelch/OPC_Farallons_end_products_final/main/epac_metadata/2023/04/metadata_2023-04-01.csv

library(RCurl)
library(tidyverse)
library(glue)

download_git=function(username,repository_name,file_path,save_dir){
  save_name=file_path %>% 
    str_split(.,"/") %>% 
    unlist() %>% 
    .[length(.)]
  
  git_url=glue("https://raw.githubusercontent.com/{username}/{repository_name}/main/{file_path}")
  file = glue("{save_dir}/{save_name}")
  f = CFILE(file,mode="wb")
  a=curlPerform(url=git_url,writedata=f@ref,noprogress=FALSE, .opts = RCurl::curlOptions(ssl.verifypeer=FALSE))
  close(f)
}

username="HeatherWelch"
repository_name="OPC_Farallons_end_products_final"
save_dir=getwd()
file_path="epac_metadata/2023/04/metadata_2023-04-15.csv"

download_git(username = username,repository_name = repository_name, file_path = file_path, save_dir = save_dir)


<img src="inst/imgs/nastyverse.png?raw=True" width="100">

