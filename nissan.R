nissan_models <- function(make){
    
    model <- gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    if(grepl("350",make,ignore.case = TRUE)  ){
        model = "Z-350"
    }else if(grepl("370",make,ignore.case = TRUE) ){
        model = "Z-370"
    }else if(grepl("al.*ma",make,ignore.case = TRUE)){
        model = "Altima"
    }else if(grepl("ar.*da",make,ignore.case = TRUE) ){
        model = "Armada"
    }else if(grepl("cube",make,ignore.case = TRUE)){
        model = "cube"
    }else if(grepl("terra",make,ignore.case = TRUE) ){
        model = "X-Terra"
    }else if(grepl("fro.*",make,ignore.case = TRUE) ){
        model = "Frontier"
    }else if(grepl("juke",make,ignore.case = TRUE) ){
        model = "Juke"
    }else if(grepl("kicks",make,ignore.case = TRUE) ){
        model = "Kicks"
    }else if(grepl("leaf",make,ignore.case = TRUE) ){
        model = "Leaf"
    }else if(grepl("mu.*no",make,ignore.case = TRUE)||
             grepl("murano",make,ignore.case = TRUE)||
             grepl("rano",make,ignore.case = TRUE)){
        model = "Murano"
    }else if(grepl("path",make,ignore.case = TRUE) ){
        model = "Pathfinder"
    }else if(grepl("q.*st",make,ignore.case = TRUE) ){
        model = "Quest"
    }else if(grepl("ro.*ge",make,ignore.case = TRUE) || 
             grepl("ro.*gue",make,ignore.case = TRUE)){
        model = "Rogue"
    }else if(grepl("se.*ra",make,ignore.case = TRUE) ){
        model = "Sentra"
    }else if(grepl("ti.*an",make,ignore.case = TRUE) ){
        model = "Titan"
    }else if(grepl("versa",make,ignore.case = TRUE) ){
        model = "versa"
    }
    
    
    return(model);
    
}

nissan_trims <- function(make,model){
    trim = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
    #trim = "Base"
    
    
    
    if( grepl("SL",make,ignore.case = TRUE)){
        trim = "SL"
    }else if(grepl("SV",make,ignore.case = TRUE)){
        trim = "SV"
    }else if(grepl("SR",make,ignore.case = TRUE)){
        trim = "SR"
    }else if(grepl("S",make,ignore.case = TRUE)){
        trim = "S"
    }else if(grepl("platinum",make,ignore.case = TRUE)){
        trim = "Platinum"
    }
    
    if(trim==""){
        trim="Base"
    }
    
    
    
    
    return(trim);
    
}
