chevy_models <- function(make){
    
    model <- gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    if(grepl("abalanche",make,ignore.case = TRUE) ||
       grepl("avalanche",make,ignore.case = TRUE) ||
       grepl("avalance",make,ignore.case = TRUE) ){
        model = "Avalanche"
    }else if(grepl("bus",make,ignore.case = TRUE)||
             grepl("express",make,ignore.case = TRUE)||
             grepl("kodiak",make,ignore.case = TRUE)||
             grepl("g4500",make,ignore.case = TRUE) ){
        model = "express"
    }else if(grepl("aveo",make,ignore.case = TRUE)||
             grepl("aveo5",make,ignore.case = TRUE)||
             grepl("aveo5lt",make,ignore.case = TRUE) ){
        model = "Aveo"
    }else if(grepl("camara",make,ignore.case = TRUE) ||
             grepl("camaro",make,ignore.case = TRUE)||
             grepl("camro",make,ignore.case = TRUE)||
             grepl("camero",make,ignore.case = TRUE)){
        model = "Camaro"
    }else if(grepl("cavailer",make,ignore.case = TRUE)||
             grepl("cavalier",make,ignore.case = TRUE)||
             grepl("caviler",make,ignore.case = TRUE)||
             grepl("cavilier",make,ignore.case = TRUE)||
             grepl("cavlier",make,ignore.case = TRUE)){
        model = "Cavalier"
    }else if(grepl("cruise",make,ignore.case = TRUE) ||
             grepl("cruize",make,ignore.case = TRUE) ||
             grepl("cruse",make,ignore.case = TRUE) ||
             grepl("cruz",make,ignore.case = TRUE) ||
             grepl("cruze",make,ignore.case = TRUE) ||
             grepl("f j",make,ignore.case = TRUE)){
        model = "Cruze"
    }else if(grepl("astro",make,ignore.case = TRUE) ){
        model = "Astrovan"
    }else if(grepl("cap.*vi?a",make,ignore.case = TRUE) ){
        model = "Captiva"
    }else if(grepl("si?lv?.*o",make,ignore.case = TRUE)||
             grepl("1500",make,ignore.case = TRUE)||
             grepl("2500",make,ignore.case = TRUE)||
             grepl("3500",make,ignore.case = TRUE)||
             grepl("4500",make,ignore.case = TRUE)||
             grepl("5500",make,ignore.case = TRUE)||
             grepl("si.*ado",make,ignore.case = TRUE)){
        model = "Silverado"
        if( grepl("1500",make,ignore.case = TRUE)){
            model = paste(model,"1500")
        }else if(grepl("2500",make,ignore.case = TRUE)){
            model = paste(model,"2500")
        }else if(grepl("3500",make,ignore.case = TRUE)){
            model = paste(model,"3500")
        }else if(grepl("4500",make,ignore.case = TRUE)){
            model = paste(model,"4500")
        }else if(grepl("5500",make,ignore.case = TRUE)){
            model = paste(model, "5500")
        }
    }else if(grepl("bl?azer?",make,ignore.case = TRUE) ){
        model = "Trailblazer"
    }else if(grepl("trav",make,ignore.case = TRUE) ){
        model = "Traverse"
    }else if(grepl("monte",make,ignore.case = TRUE) ){
        model = "Montecarlo"
    }else if(grepl("ma.*bu\\.?",make,ignore.case = TRUE) ){
        model = "Malibu"
    }else if(grepl("su.*r.*ban",make,ignore.case = TRUE) ){
        model = "Suburban"
    }else if(grepl(".*ala",make,ignore.case = TRUE) ){
        model = "Impala"
    }else if(grepl("corv.*",make,ignore.case = TRUE) ){
        model = "Corvette"
    }else if(grepl("co.*b.*lt",make,ignore.case = TRUE) ){
        model = "Cobalt"
    }else if(grepl("eq.*n.*o(z|x)",make,ignore.case = TRUE) ){
        model = "Equinox"
    }
    
    
    return(model);
    
}

chevy_trims <- function(make,model){
    #trim = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
    trim = "Base"
   
    
    
    if( grepl("1500",make,ignore.case = TRUE)){
        trim = "1500"
    }else if(grepl("2500",make,ignore.case = TRUE)){
        trim = "2500"
    }else if(grepl("3500",make,ignore.case = TRUE)){
        trim = "3500"
    }else if(grepl("4500",make,ignore.case = TRUE)){
        trim = "4500"
    }else if(grepl("5500",make,ignore.case = TRUE)){
        trim = "5500"
    }
    
    if(grepl("ltz",make,ignore.case = TRUE) ){
        trim = paste(trim,"LTZ")
    }else if(grepl("LS",make,ignore.case = TRUE) ){
        trim = paste(trim,"LS")
    }else if(grepl("LT",make,ignore.case = TRUE) ){
        trim = paste(trim,"LT")
    }
    
    if(grepl("z71",make,ignore.case = TRUE) ){
        trim = paste(trim,"Limited",sep = " ")
    }
    if(grepl("4x4",make,ignore.case = TRUE) ){
        trim = paste(trim,"4X4",sep = " ")
    }
    if(grepl("hd",make,ignore.case = TRUE) ){
        trim = paste(trim,"HD",sep = " ")
    }
    if(grepl("sport",make,ignore.case = TRUE) ){
        trim = paste(trim,"sport",sep = " ")
    }
    
    
    if(trim==""){
        trim="Base"
    }
    
    
    
    
    return(trim);
    
}
