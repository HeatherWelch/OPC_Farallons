# https://www.techielass.com/convert-a-folder-to-a-git-repository/
cd /Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/end_products
git init
git add .
git commit -m "add my files"
git remote add origin https://github.com/HeatherWelch/OPC_Farallons_end_products_final
# git branch --set-upstream-to=origin/ main
# git pull --set-upstream origin main
# git pull origin main
git pull origin main --allow-unrelated-histories
# https://stackoverflow.com/questions/19085807/please-enter-a-commit-message-to-explain-why-this-merge-is-necessary-especially
# git config pull.rebase false 
git push -u origin main
username: HeatherWelch
password: personal access token (stickies)

## commiting new stuff in folder
cd /Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/end_products
git add .
git commit -m "Added foo to the bar1"
git push

## auto sync
# https://github.com/GitJournal/git-auto-sync
git config --global user.name "Heather Welch"
git config --global user.email "heather.welch@noaa.gov"
git-auto-sync sync
git-auto-sync daemon add 
git-auto-sync daemon status
git-auto-sync daemon remove 

setwd("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/end_products")
system("git-auto-sync sync")


https://github.com/HeatherWelch/OPC_Farallons_end_products/main

### important:
1. Allow terminal, bash, and cron full disk access

## crons 
1. Open terminal\
2. Create new cron tab using nano as an editor:\
env EDITOR=nano crontab -e\
3. Create cron job, format = (timing), pathway to Rscript, pathway to scripttorun.r:\
52 11 * * * /Users/EcoCast/Dropbox/OPC_Farallons/github/OPC_Farallons/Operationalizing_V2/run_OPC_farallons_annex > /Users/EcoCast/Dropbox/OPC_Farallons/operationalization/intermediate/cron_logs/opc_cron_log_annex 2>&1
05 12 * * * rscript /Users/EcoCast/Dropbox/OPC_Farallons/github/OPC_Farallons/Operationalizing_V2/AutoSync.R


if you don't know where Rscript is, enter : which Rscript into terminal\
4. control o (saves new line of text)\
5. hit return (writes new line of text to cron tab)\
6. control x (exits out of cron tab)\

## running an exe in terminal
give cron, terminal, and bash full disk access
The file run_OPC_farallons2.txt needs to have execute permissions if it doesn't already. ( chmod +x run_OPC_farallons2.txt )
