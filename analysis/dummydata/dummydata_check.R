dummydata_check <- function(
  dummydata_studydef,
  dummydata_custom
) {

  not_in_studydef <- names(dummydata_custom)[!( names(dummydata_custom) %in% names(dummydata_studydef) )]
  not_in_custom  <- names(dummydata_studydef)[!( names(dummydata_studydef) %in% names(dummydata_custom) )]
  
  
  if(length(not_in_custom)!=0) stop(
    paste(
      "These variables are in studydef but not in custom: ",
      paste(not_in_custom, collapse=", ")
    )
  )
  
  if(length(not_in_studydef)!=0) {
    print(
      paste(
        "These variables are not in studydef so will be removed from custom: ",
        paste(not_in_studydef, collapse=", ")
      )
    )
  }
  
  # reorder columns
  dummydata_custom <- dummydata_custom[,names(dummydata_studydef)]
  
  coltypes_studydef <- map_chr(dummydata_studydef, ~paste(class(.), collapse=", "))
  coltypes_custom <- map_chr(dummydata_custom, ~paste(class(.), collapse=", "))
  
  unmatched_types <- cbind(
    coltypes_studydef, 
    coltypes_custom
    )[coltypes_studydef != coltypes_custom, ] %>%
    as.data.frame() %>% rownames_to_column()
  
  if(nrow(unmatched_types)>0) stop(
    #unmatched_types
    "inconsistent typing in studydef : dummy dataset\n",
    apply(unmatched_types, 1, function(row) paste(paste(row, collapse=" : "), "\n"))
  )
  
  print("Studydef and custom dummy data now match!")
  
  return(dummydata_custom)
  
}
