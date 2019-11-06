toyota_models <- function(make){
    
    model <- NA
    if(grepl("4run",make,ignore.case = TRUE) || 
       grepl("4 run",make,ignore.case = TRUE) ){
        model = "4 Runner"
    }else if(grepl("aval",make,ignore.case = TRUE) ){
        model = "Avalon"
    }else if(grepl("cam",make,ignore.case = TRUE) ){
        model = "Camry"
    }else if(grepl("rol",make,ignore.case = TRUE) ){
        model = "Corolla"
    }else if(grepl("fj",make,ignore.case = TRUE) ||
             grepl("f j",make,ignore.case = TRUE)){
        model = "FJ Cruiser"
    }else if(grepl("land[ ]?cru",make,ignore.case = TRUE) ){
        model = "Land Cruiser"
    }else if(grepl("lander",make,ignore.case = TRUE) ){
        model = "Highlander"
    }else if(grepl("m.*ix",make,ignore.case = TRUE) ){
        model = "Matrix"
    }else if(grepl("pri",make,ignore.case = TRUE) ){
        model = "Prius"
    }else if(grepl("seq",make,ignore.case = TRUE) ){
        model = "Sequoia"
    }else if(grepl("sien",make,ignore.case = TRUE) ){
        model = "Sienna"
    }else if(grepl("sol",make,ignore.case = TRUE) ){
        model = "Solara"
    }else if(grepl("taco",make,ignore.case = TRUE) ){
        model = "Tacoma"
    }else if(grepl("t[h]?und",make,ignore.case = TRUE) ){
        model = "Tundra"
    }else if(grepl("enza",make,ignore.case = TRUE) ){
        model = "Venza"
    }else if(grepl("yaris",make,ignore.case = TRUE) ){
        model = "Yaris"
    }else if(grepl("86",make,ignore.case = TRUE) ){
        model = "86"
    }else if(grepl("supra",make,ignore.case = TRUE) ){
        model = "Supra"
    }
    
    return(model);
    
}

toyota_trims <- function(make,model){
    #trim = gsub(".*[ ](.*)?","\\1",make,ignore.case = TRUE)
    if(grepl(" ",make)){
        trim = gsub(".*[ ](.*)?","\\1",make,ignore.case = TRUE)
    }else{
        trim = ""
    }
    
    
    if(grepl("runner",make,ignore.case = TRUE) ){
        trim = "SR5"
    }
    if(grepl("prerunner",make,ignore.case = TRUE) ){
        trim = "prerunner"
    }
    if(grepl("crewmax",make,ignore.case = TRUE) ){
        trim = "Crewmax"
    }
    if(grepl("2wd",make,ignore.case = TRUE) ){
        trim = "2WD"
    }
    if(grepl("4WD",make,ignore.case = TRUE) ){
        trim = "4WD"
    }
    if(grepl("4x4",trim,ignore.case = TRUE) ){
        trim = ""
    }
    
    if(grepl("Touring",trim,ignore.case = TRUE) ){
        trim = "Touring"
    }else if(grepl("XLE",trim,ignore.case = TRUE) ){
        trim = "XLE"
    }else if(grepl("XLS",trim,ignore.case = TRUE) ){
        trim = "XLS"
    }else if(grepl("SLE",trim,ignore.case = TRUE) ){
        trim = "SLE"
    }else if(grepl("XSE",trim,ignore.case = TRUE) ){
        trim = "XSE"
    }else if(grepl("SR5",trim,ignore.case = TRUE) ){
        trim = "SR5"
    }else if(grepl("TRD",trim,ignore.case = TRUE) ){
        trim = "TRD"
    }else if(grepl("Hybrid",trim,ignore.case = TRUE) ){
        trim = "Hybrid"
    }else if(grepl("LE",trim,ignore.case = TRUE) ){
        trim = "LE"
    }else if(grepl("SE",trim,ignore.case = TRUE) ){
        trim = "SE"
    }else if(grepl("S",trim,ignore.case = TRUE)){
        trim = "S"
    }else if(grepl("L",trim,ignore.case = TRUE) ){
        trim = "L"
    }
    
    if(grepl("Limited",make,ignore.case = TRUE) ||
       grepl("ltd",make,ignore.case = TRUE)){
        trim = paste(trim,"Limited",sep = " ")
    }
    if(grepl("sport",make,ignore.case = TRUE) ){
        trim = paste(trim,"Sport",sep = " ")
    }
    if(grepl("4x4",make,ignore.case = TRUE) ){
        trim = paste(trim,"4X4",sep = " ")
    }
    if(grepl("Pro",make,ignore.case = TRUE) ){
        trim = paste(trim,"Pro",sep = " ")
    }
    if(grepl("v6",make,ignore.case = TRUE) ){
        trim = paste(trim,"V6",sep = " ")
    }
    if(grepl("plus",make,ignore.case = TRUE) ){
        trim = paste(trim,"Plus",sep = " ")
    }
    
    if(trim==""){
        trim="Base"
    }
    
    
   
    
   return(trim);
    
}
