# Write a schedule file writer function with three sub functions 
# write header 
# write block header
# write a block 

write_sch_block <- function(values, labels, value_width = 14) {
  fmt <- paste0("%-", value_width, "s%s")
  paste(sprintf(fmt, as.character(values), labels), collapse = "\n")
}



writeSchedule <- function(siteTable, siteEventTable){
    
    tempHeader = writeHeader(siteTable)
    tempBlockHeader = writeBlockHeader(siteTable)
    tempBlock = writeBlock(siteEventTable)

    outfile = cat(Header, blockHead, block, "-9999")

}

writeHeader <- function(siteTable){

    startYear   = siteTable$startYear
    endYear     = siteTable$endYear
    siteFile    = siteTable$siteFile
    intalSystem = siteTable$intialSystem
    intalCrop   = siteTable$intialCrop
    intalTree   = siteTable$intialTree

    vals <- list(startYear, endYear, siteFile, 0, -1, "-1.00",
             -1, -1, -1, 0, 0, -1, intialSystem, intialCrop, intialTree)
    labs <- c("Starting year","Last year","Site file name","Labeling type",
          "Labeling year","Microcosm","CO2 Systems","pH effect",
          "Soil Warming","N input scalar option","OMAD scalar option",
          "Climate scalar option","Initial system","Initial crop",
          "Initial tree")

    outHeader = cat(write_sch_block(vals, labs))
    
}

writeBlockHeader <- function(siteTable){

    startYear   = siteTable$startYear
    endYear     = siteTable$endYear
    siteFile    = siteTable$siteFile
    intalSystem = siteTable$intialSystem
    intalCrop   = siteTable$intialCrop
    intalTree   = siteTable$intialTree

    vals <- list(startYear, endYear, siteFile, 0, -1, "-1.00",
             -1, -1, -1, 0, 0, -1, intialSystem, intialCrop, intialTree, "","  Year Month Option")
    labs <- c("Starting year","Last year","Site file name","Labeling type",
          "Labeling year","Microcosm","CO2 Systems","pH effect",
          "Soil Warming","N input scalar option","OMAD scalar option",
          "Climate scalar option","Initial system","Initial crop",
          "Initial tree", "", "")

    outHeader <- cat(write_sch_block(vals, labs))
}

writeBlock <- function(siteEventTable, 
                        outputMonth = 12, 
                        outputInterval = 0.083,
                        blockNumber = 1 
                        ){

    startYear   = siteTable$startYear
    endYear     = siteTable$endYear
    Repeats    = endYear - startYear
    weatherFile = siteTable$weatherFile
    if(is.null(weatherFile)){
        weatherChoice = "C"
    }else{
        weatherChoice = "F"
        }


    vals <- list(blockNumber, endYear, Repeats, 
                 startYear, outputMonth, outputInterval, weatherChoice)
                 
    labs <- c(
        "Block", "Last year", "Repeats # years","Output starting year",
        "Output month","Output interval","Weather choice")
    
    outHeader <- cat(write_sch_block(vals, labs))

    if(!is.null(weatherFile)){
        cat(outHeader, weatherFile)
    }
}

buildFile <- function(Header, blockHead, block){
    outfile = cat(Header, blockHead, block, "-9999")
}


