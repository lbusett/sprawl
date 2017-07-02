require(tidyverse)
require(lubridate)
require(RJSONIO)
require(data.table)
require(httr)
#   ____________________________________________________________________________
#   Set Variables                                                           ####

user = "planetek"
pwd =  "TJBVtCQM"

# RE tile IDS
tile_ids =  data.frame(tiles = c(3260111,3260112,3260113,3260114,3260115,3260116,3260117,3260011,3260012,3260013,3260014,
                                 3260015,3260016,3260017,3259911,3259912,3259913,3259914,3259915,3259916,3259917,3259811,
                                 3259812,3259813,3259814,3259815,3259816,3259817,3259711,3259712,3259713,3259714,3259715,
                                 3259716,3259717)) %>% 
  as_tibble()
# output directory
out_dir = "/home/gcandiani/nas-4a/Vegetables/PROJECTS/TELEMOD2017/1_DATA/IMAGES/RE"
idl_dir = "/home/lb/Temp/buttami/pippo"
idl_scripts_dir = "/home/gcandiani/idl/"

out_dir = "/home/lb/Temp/buttami/pippo"
dir.create(out_dir)


#   ____________________________________________________________________________
#   Set process varaibles                                                   ####

date <- Sys.Date() - 300
date1 <- date - 7

# create URL

url = paste0("https://eyefind-ag.blackbridge.com/api/v1.3?toiStart=", 
             Sys.Date() - 307,
             "&toiEnd=", 
             Sys.Date() - 300, 
             "&blkFill=0&cc=20&imgType=itt&aoiType=ids&idType=tid&idList=", 
             paste(tile_ids$tiles, collapse = "," ))

url_res <- GET(url, authenticate(user, pwd)) 

if (url_res$status_code == 200) {
  
  #   ____________________________________________________________________________
  #   Get the list of files                                             ---   ####
  #   
  json_res <- content(url_res, "parsed")
  feats <- json_res$features %>% 
    rbindlist() %>% 
    select(name, prodUrl)
  
  for (file_ind in seq_along(along = feats$name)) {
    
    outfile <- file.path(out_dir,paste0(feats$name[file_ind], ".zip"))
    
    if (file.exists(outfile) == FALSE) {
      
      #   ____________________________________________________________________________
      #   Download file if not existing                                     ---   ####
      #   
      
      GET(feats$prodUrl[file_ind], write_disk(outfile, overwrite=TRUE), authenticate(user, pwd))
      
      aria_string <- paste(Sys.which("aria2c"), " -x 6 -d ", out_dir,
                           " -o ", feats$prodUrl[file_ind],
                           " --allow-overwrite --file-allocation=none --retry-wait=2",
                           " --http-user=", user, " --http-passwd=", pwd, sep = "") 
      unzip(outfile, exdir = file.path(out_dir))
      
      
      #   ____________________________________________________________________________
      #   Create the batch file needed to run the TELEMOD2.PRO IDL funtion ----   ####
      #   from a command shell
      #   
      exp_path_str <- paste0(
        "!PATH = Expand_Path('", "+", 
        idl_scripts_dir, 
        "') +' ;' + !PATH"
      )  
      
      str_idl <- paste0(
        "res = TELEMOD2"
      )
      
      batch_file <- file.path(idl_dir, "Reye_batch.pro")
      fileConn   <- file(batch_file)
      writeLines(c(exp_path_str, 
                   "envi, /restore_base_save_files  ", 
                   "ENVI_batch_init", 
                   str_idl, 
                   "exit"), fileConn)
      close(fileConn)
      
      
      #   ____________________________________________________________________________
      #   Execute FRG_Create_ROI_batch.pro                                        ####
      
      out <- system2("idl.exe", args = batch_file, stdout = "log_IDL")
      
    }
    
  }
  
}

