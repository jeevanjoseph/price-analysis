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
        trim = gsub(".*[ ](.*)?","\\1",paste(make,"base",sep = " "),ignore.case = TRUE)
    }
    if(grepl("limit",make,ignore.case = TRUE) || 
       grepl("ltd",make,ignore.case = TRUE) ){
        trim = "Limited"
    }else if(grepl("platinum",make,ignore.case = TRUE) ){
        trim = "Platinum"
    }else if(grepl("xlt",make,ignore.case = TRUE) ){
        trim = "XLT"
    }else if(grepl("lariat",make,ignore.case = TRUE) ){
        trim = "Lariat"
    }else if(grepl("king",make,ignore.case = TRUE) ){
        trim = "King Ranch"
    }else if(grepl("se",make,ignore.case = TRUE) ){
        trim = "SE"
    }else if(grepl("sel",make,ignore.case = TRUE) ){
        trim = "SEL"
    }else if(grepl("titanium",make,ignore.case = TRUE)){
        trim = "Titanium"
    } else if(grepl("xls",make,ignore.case = TRUE)){
        trim = "XLS"
    }else if(grepl("shelby",make,ignore.case = TRUE)){
        trim = "Shelby"
    }else if(grepl("xl",make,ignore.case = TRUE)){
        trim = "XL"
    }else if(grepl("lx",make,ignore.case = TRUE)){
        trim = "LX"
    }else if(grepl("eddie",make,ignore.case = TRUE)){
        trim = "Eddie Bauer"
    }else if(grepl("4x4",make,ignore.case = TRUE)){
        trim = "4x4"
    }else if(grepl("fx4",make,ignore.case = TRUE)){
        trim = "FX4"
    }else if(grepl("fx2",make,ignore.case = TRUE)){
        trim = "FX2"
    }else if(grepl("sport",make,ignore.case = TRUE)){
        trim = "Sport"
    }else if(grepl("gt",make,ignore.case = TRUE)){
        trim = "GT"
    }else if(grepl("zx2",make,ignore.case = TRUE)){
        trim = "ZX2"
    }else if(grepl("ZX3",make,ignore.case = TRUE)){
        trim = "ZX3"
    }else if(grepl("svt",make,ignore.case = TRUE)){
        trim = "SVT"
    }else if(grepl("sd",make,ignore.case = TRUE) ||
             grepl("super duty",make,ignore.case = TRUE)){
        trim = "Super Duty"
    }else if(grepl("150",make,ignore.case = TRUE)||
             grepl("250",make,ignore.case = TRUE)||
             grepl("350",make,ignore.case = TRUE)){
        trim = "base"
    }
    
    return(trim);
    
}
