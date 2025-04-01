install_if_not_installed <- function(package_list){
    
    uninstalled_pkgs <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    if(length(uninstalled_pkgs)) install.packages(uninstalled_pkgs)
    }

install_if_not_installed(c("ggplotify", "imager"))

library(tidyverse)
library(stringr)
library(imager)
library(ggplotify)

###########
default_bucket = Sys.getenv('WORKSPACE_BUCKET')
default_directory = ''

README <- function()
    print("
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ README: How to use gcr_data_storage?~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
gcr_data_storage lets you easily move data between your development environment (e.g. Jupyter Notebook) and your Google Cloud Workspace bucket. 
It integrates the command line tool gsutil.

  * Use `save_data_to_bucket(data, filename)` to save data from your development environment to the bucket.

  * Use `read_data_to_bucket(filename)` to read data from the bucket into your development environment, with the option to keep a copy in the disk.

  * Use `copy_from_bucket_to_bucket(origin_filename, destination_bucket)` to copy data between different directories within the same bucket or between two different buckets owned by the user.

  * Use `list_saved_data()` to obtain a list of data saved in the bucket or the disk.

gcr_data_storage was originally written to be used within the All of Us Researcher Workbench environment but can be used in other Google Cloud Environments.

    ```
    # Example code
    source(url('https://github.com/AymoneKouame/r-google-cloud-data-storage/gcr_data_storage.r'))

    ## list data in the bucket root directory 
    list_saved_data()
    ```
More information, including examples, at https://github.com/AymoneKouame/r-google-cloud-data-storage/ .
    ")
                    
save_data_to_bucket <- function(data
                                , filename
                                , directory=NULL
                                , scale = 1
                                , width = NA
                                , height = NA
                                , units = c("in", "cm", "mm", "px")
                                , dpi = 300
                                , limitsize = TRUE
                                , bucket =NULL){
    
    if (is.null(directory)== TRUE){directory = default_directory}
    if (is.null(bucket)== TRUE){bucket = default_bucket}
    
    print(str_glue("
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Saving data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To location =  '{bucket}/{directory}'
    "))

    full_filename <- str_glue('{bucket}/{directory}/{filename}') 

    if (any(class(data) %in% c('tbl_df','tbl','data.frame')) == TRUE){
        write.table(data, filename)

    } else if (any(class(data) %in% c('gg','ggplot')) == TRUE){
        ggsave(as.ggplot(race_bplot), filename = filename
               , scale = scale, width = width, height = height
               , units = units, dpi = dpi, limitsize = limitsize)         
    } else { 
        print("
Your file extension is not a dataframe or plot. We will assume your data is already saved to your disk.\n")
        
        }      
    
    res <- system(str_glue("gsutil cp {filename} {full_filename}"), intern= F)
    print(str_glue("Data saved as '{filename}' in location."))
    res_rm <- system(str_glue("rm {filename}"))
  }


read_data_from_bucket<- function(filename
                                 , bucket = NULL
                                 , directory= NULL
                                 , save_copy_in_disk = TRUE){
    
    if (is.null(directory)== TRUE){directory = default_directory}
    if (is.null(bucket)== TRUE){bucket = default_bucket}
    
    print(str_glue("
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Reading data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
From location =  '{bucket}/{directory}'
    "))
    
    full_filename <- str_glue('{bucket}/{directory}/{filename}') 
    res <- system(str_glue("gsutil cp {full_filename} {filename}"), intern=F)
    
    if (grepl(pattern = '.png|.jpeg|.bmp|.tiff|.pdf|.emf', filename)){
        data <- load.image(file = filename)

       ### df <- read_parquet(filename, as_tibble = FALSE) ADD CODE

    } else if (grepl(pattern = '.csv|.xlsx|.parquet|.tsv', filename)){
         data <- as.data.frame(read.table(filename)) 

    } else {
        data = "Your file extension is not supported. Your data was just be copied to the persistent disk."}
    
    if (save_copy_in_disk == TRUE){print(str_glue("'{filename}' is in the persistent disk."))
    } else if (save_copy_in_disk == FALSE) {system(str_glue("rm {filename}"))}

    return(data)
}

copy_from_bucket_to_bucket<- function(origin_filename
                                       , destination_bucket
                                       , origin_bucket = NULL
                                       , origin_directory = NULL
                                       , destination_directory = NULL
                                       , destination_filename = NULL
                                       ){

        if (is.null(destination_filename)==TRUE){destination_filename = origin_filename}
        if (is.null(origin_bucket)==TRUE){origin_bucket = default_bucket}
        if (is.null(origin_directory)==TRUE){origin_directory = default_directory}
        if (is.null(destination_directory)==TRUE){destination_directory = default_directory}
       
        origin_fullfilename = gsub('//','/', str_glue("{origin_bucket}/{origin_directory}/{origin_filename}"))
        origin_fullfilename = gsub('gs:/','gs://',origin_fullfilename)
    
        dest_fullfilename = gsub('//','/', str_glue("{destination_bucket}/{destination_directory}/{destination_filename}"))
        dest_fullfilename = gsub('gs:/','gs://', dest_fullfilename)                                   

        print(str_glue("
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ copying data between buckets ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    From {origin_fullfilename}
    To {dest_fullfilename}
        "))
    
     res <- system(str_glue("gsutil cp {origin_fullfilename} {dest_fullfilename}"), intern= F)
    
     if (res ==1){print('Data successfully copied.')}

    }

list_saved_data<- function(bucket_or_disk = 'bucket'
                           , bucket = NULL
                           , directory = NULL
                           , pattern = '*'){
    
    print("
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Listing data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ")
    
    if (tolower(bucket_or_disk) %in% c('bucket','')){
        if (is.null(directory) == TRUE){directory = default_directory}
        if (is.null(bucket)== TRUE){bucket = default_bucket}
        location = gsub('//', '/', str_glue("{bucket}/{directory}/{pattern}"))
        location = gsub('gs:/', 'gs://', location)
        print(str_glue('In {location}\n'))
        system(str_glue("gsutil ls {location}"), intern=T) 
                
   } else if ((tolower(bucket_or_disk) %in% c('persistent disk','persistent_disk', 'disk')) 
             | (!tolower(bucket_or_disk) %in% c('bucket',''))){
        if (is.null(directory)== TRUE){directory = ''}
        location = gsub('/*', '', str_glue("{directory}/{pattern}"))
        print(str_glue('In disk {location}\n\n'))
        system(str_glue("ls {directory}{pattern}"), intern=T)
    
     }
    }
