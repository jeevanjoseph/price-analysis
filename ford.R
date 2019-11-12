ford_models <- function(make){
    model= gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    if(grepl("e-150",make,ignore.case = TRUE) || 
       grepl("e150",make,ignore.case = TRUE) || 
       grepl("e 150",make,ignore.case = TRUE) ){
        model = "e-150"
    }else if(grepl("e-250",make,ignore.case = TRUE) || 
             grepl("e250",make,ignore.case = TRUE) || 
             grepl("e 250",make,ignore.case = TRUE) ){
        model = "e-250"
    }else if(grepl("e-350",make,ignore.case = TRUE) || 
             grepl("e350",make,ignore.case = TRUE) || 
             grepl("e 350",make,ignore.case = TRUE) ){
        model = "e-350"
    }else if(grepl("e-450",make,ignore.case = TRUE) || 
             grepl("e450",make,ignore.case = TRUE) || 
             grepl("e 450",make,ignore.case = TRUE) ){
        model = "e-450"
    }else if(grepl("f-150",make,ignore.case = TRUE) || 
             grepl("f150",make,ignore.case = TRUE) || 
             grepl("f 150",make,ignore.case = TRUE)){
        model = "f-150"
    }else if(grepl("f-250",make,ignore.case = TRUE) || 
             grepl("f250",make,ignore.case = TRUE) || 
             grepl("f 250",make,ignore.case = TRUE)){
        model = "f-250"
    }else if(grepl("f-350",make,ignore.case = TRUE) || 
             grepl("f350",make,ignore.case = TRUE) || 
             grepl("f 350",make,ignore.case = TRUE)){
        model = "f-350"
    }else if(grepl("f-450",make,ignore.case = TRUE) || 
             grepl("f450",make,ignore.case = TRUE) || 
             grepl("f 450",make,ignore.case = TRUE)){
        model = "f-450"
    }else if(grepl("f-550",make,ignore.case = TRUE) || 
             grepl("f550",make,ignore.case = TRUE) || 
             grepl("f 550",make,ignore.case = TRUE)){
        model = "f-550"
    }else if(grepl("f-650",make,ignore.case = TRUE) || 
             grepl("f650",make,ignore.case = TRUE) || 
             grepl("f 650",make,ignore.case = TRUE)){
        model = "f-650"
    }else if(grepl("f-750",make,ignore.case = TRUE) || 
             grepl("f750",make,ignore.case = TRUE) || 
             grepl("f 750",make,ignore.case = TRUE)){
        model = "f-750"
    }else if(grepl("transit-150",make,ignore.case = TRUE) || 
             grepl("t150",make,ignore.case = TRUE) || 
             grepl("t 150",make,ignore.case = TRUE) || 
             grepl("t-150",make,ignore.case = TRUE) || 
             grepl("transit 150",make,ignore.case = TRUE) ||
             grepl("transit150",make,ignore.case = TRUE)){
        model = "t-150"
    }else if(grepl("transit-250",make,ignore.case = TRUE) || 
             grepl("t250",make,ignore.case = TRUE) || 
             grepl("t 250",make,ignore.case = TRUE) || 
             grepl("t-250",make,ignore.case = TRUE) || 
             grepl("transit 250",make,ignore.case = TRUE) ||
             grepl("transit250",make,ignore.case = TRUE)){
        model = "t-250"
    }else if(grepl("transit-350",make,ignore.case = TRUE) || 
             grepl("t350",make,ignore.case = TRUE) || 
             grepl("t 350",make,ignore.case = TRUE) || 
             grepl("t-350",make,ignore.case = TRUE) || 
             grepl("transit 350",make,ignore.case = TRUE) ||
             grepl("transit350",make,ignore.case = TRUE)){
        model = "t-350"
    }else if(grepl("cmax",make,ignore.case = TRUE) || 
             grepl("c-max",make,ignore.case = TRUE)){
        model = "C-Max"
    }else if(grepl("msutang",make,ignore.case = TRUE) || 
             grepl("muatang",make,ignore.case = TRUE) ||
             grepl("mustamg",make,ignore.case = TRUE) ||
             grepl("mustand",make,ignore.case = TRUE) ||
             grepl("mustang",make,ignore.case = TRUE) ||
             grepl("mustanvertible",make,ignore.case = TRUE) ||
             grepl("mustatng",make,ignore.case = TRUE) ||
             grepl("mustrang",make,ignore.case = TRUE)){
        model = "Mustang"
    }else if(grepl("tarus",make,ignore.case = TRUE) || 
             grepl("tauras",make,ignore.case = TRUE) ||
             grepl("taurs",make,ignore.case = TRUE) ||
             grepl("taurus",make,ignore.case = TRUE)){
        model = "Taurus"
    }else if(grepl("exporer",make,ignore.case = TRUE) ||
             grepl("explorer",make,ignore.case = TRUE) ||
             grepl("explore",make,ignore.case = TRUE) ||
             grepl("exploler",make,ignore.case = TRUE) ||
             grepl("exolore",make,ignore.case = TRUE) ||
             grepl("exlorer",make,ignore.case = TRUE)){
        model = "Explorer"
    } else if (grepl("expidition",make,ignore.case = TRUE) ||
               grepl("expetidition",make,ignore.case = TRUE) ||
               grepl("expeidtion",make,ignore.case = TRUE) ||
               grepl("expedtion",make,ignore.case = TRUE) ||
               grepl("expediton",make,ignore.case = TRUE) ||
               grepl("expeditionex",make,ignore.case = TRUE) ||
               grepl("expedition,eddie",make,ignore.case = TRUE) ||
               grepl("expedition",make,ignore.case = TRUE) ||
               grepl("exp",make,ignore.case = TRUE)){
        model = "Expedition"
    }else if(grepl("150",make,ignore.case = TRUE)){
        if(ford[row, "make"] == "van"){
            model = "e-150"    
        }else{
            model = "f-150"
        }
    }else if(grepl("250",make,ignore.case = TRUE)){
        if(ford[row, "make"] == "van"){
            model = "e-250"    
        }else{
            model = "f-250"
        }
    }else if(grepl("350",make,ignore.case = TRUE)){
        if(ford[row, "make"] == "van"){
            model = "e-350"    
        }else{
            model = "f-350"
        }
    }
    
    return(model);
    
}

ford_trims <- function(make,model){
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
    }else if(grepl("ecoboost",make,ignore.case = TRUE) ){
        trim = "Ecoboost"
    }else if(grepl("150",make,ignore.case = TRUE)||
             grepl("250",make,ignore.case = TRUE)||
             grepl("350",make,ignore.case = TRUE)){
        trim = "base"
    }
    
    return(trim);
    
}
