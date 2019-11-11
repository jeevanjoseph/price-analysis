honda_models <- function(make){
    
    model <- gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    if(grepl("ac.*rd",make,ignore.case = TRUE) ){
        model = "Accord"
    }else if(grepl("ci.*ic",make,ignore.case = TRUE)||
             grepl("civi",make,ignore.case = TRUE)){
        model = "Civic"
    }else if(grepl("cla",make,ignore.case = TRUE) ){
        model = "Clarity"
    }else if(grepl("cr.*v",make,ignore.case = TRUE) ){
        model = "CR-V"
    }else if(grepl("cr.*z",make,ignore.case = TRUE)){
        model = "CR-Z"
    }else if(grepl("el.*nt",make,ignore.case = TRUE)){
        model = "Element"
    }else if(grepl("fit",make,ignore.case = TRUE) ){
        model = "Fit"
    }else if(grepl("o.*d.*s.*y",make,ignore.case = TRUE) ){
        model = "Odessey"
    }else if(grepl("hr.*v",make,ignore.case = TRUE)){
        model = "HR-V"
    }else if(grepl("pa.*ort",make,ignore.case = TRUE) ){
        model = "Passport"
    }else if(grepl("pi.*ot",make,ignore.case = TRUE) ){
        model = "Pilot"
    }else if(grepl("ridge",make,ignore.case = TRUE) ){
        model = "Ridgeline"
    }else if(grepl("pre.*de",make,ignore.case = TRUE) ){
        model = "Prelude"
    }else if(grepl("s[ -]?2000",make,ignore.case = TRUE) ){
        model = "S 2000"
    }else if(grepl("shad*",make,ignore.case = TRUE) ){
        model = "Shadow"
    }
    
    
    return(model);
    
}

honda_trims <- function(make,model){
    #trim = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
    trim = "Base"

    if( grepl("LX",make,ignore.case = TRUE)){
        trim = "LX"
    }else if(grepl("Sport",make,ignore.case = TRUE)){
        trim = "Sport"
    }else if(grepl("EX[ -]?L",make,ignore.case = TRUE)){
        trim = "EX-L"
    }else if(grepl("EX",make,ignore.case = TRUE)){
        trim = "EX"
    }else if(grepl("tour",make,ignore.case = TRUE)){
        trim = "Touring"
    }
    
    if(trim==""){
        trim="Base"
    }
    return(trim);
    
}
