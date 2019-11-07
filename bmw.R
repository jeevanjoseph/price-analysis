bmw_models <- function(make){
    
    model <- NA
    model_start = substr(make,1,1)
    if(model_start == "1" || model_start == "3" ||model_start == "5" ||model_start == "7" ) {
       model <- paste(model_start,"series")
    }
    else if(model_start == "m" || model_start == "x" ||model_start == "z" ||model_start == "i") {
        ## for X and M models, model will be X{num}.
        ## whatever follows is used as a trim - will need more work to reduce the levels here.
        model <- substr(gsub("(x|m|z|i)[ \\-]?([0-9]+)","\\1\\2",make),1,2)
        #bmw[row, "trim"] = substr(gsub("(x|m)[ \\-]?([0-9]+)","\\1\\2",make),4,nchar(gsub("(x|m)[ \\-]?([0-9]+)","\\1\\2",make)))
    }else if(grepl(".*([0-9]{3}).*",make,ignore.case = TRUE)){
        model <- paste(substr(gsub(".*([0-9]{3}).*","\\1",make),1,1),"series")
    } else if(grepl(".*(m5|m3|6 series|6-series|4 series|4-series|2 series|2-series|b7|z3).*",make,ignore.case = TRUE)){
        model <- gsub(".*(m5|m3|6 series|6-series|4 series|4-series|2 series|2-series|b7|z3).*","\\1",make)
        
    }
    
   
    if(grepl("m ",model,ignore.case = TRUE)  ){
        model = "m3"
    }else if(grepl("z ",model,ignore.case = TRUE) ||
             grepl("z5",model,ignore.case = TRUE)){
        model = "z4"
    }
    
    return(model);
    
}

bmw_trims <- function(make,model){
    #trim = gsub(".*[ ](.*)?","\\1",make,ignore.case = TRUE)
    if(grepl(" ",make)){
        trim = gsub(".*[ ](.*)?","\\1",make,ignore.case = TRUE)
    }else{
        trim = "base"
    }
    if(grepl("320 ?-?i",make,ignore.case = TRUE)){
        trim = "320i"
    }else if(grepl("330 ?-?i",make,ignore.case = TRUE) ){
        trim = "330i"
    }else if(grepl("328 ?-?d",make,ignore.case = TRUE) ){
        trim = "328d"
    }else if(grepl("330 ?-?e",make,ignore.case = TRUE) ){
        trim = "330e"
    }else if(grepl("340 ?-?i",make,ignore.case = TRUE) ){
        trim = "340i"
    }else if(grepl("430 ?-?i",make,ignore.case = TRUE) ){
        trim = "430i"
    }else if(grepl("440 ?-?i",make,ignore.case = TRUE) ){
        trim = "440i"
    }else if(grepl("530 ?-?e",make,ignore.case = TRUE) ){
        trim = "530e"
    }else if(grepl("540 ?-?i",make,ignore.case = TRUE) ){
        trim = "540i"
    }else if(grepl("540 ?-?d",make,ignore.case = TRUE) ){
        trim = "540d"
    }else if(grepl("550 ?-?i",make,ignore.case = TRUE) ){
        trim = "550i"
    }else if(grepl("740 ?-?i",make,ignore.case = TRUE) ){
        trim = "740i"
    }else if(grepl("740 ?-?e",make,ignore.case = TRUE) ){
        trim = "740e"
    }else if(grepl("750 ?-?i",make,ignore.case = TRUE) ){
        trim = "750i"
    }else if(grepl("760 ?-?i",make,ignore.case = TRUE) ){
        trim = "760i"
    }
    
    return(trim);
    
}
