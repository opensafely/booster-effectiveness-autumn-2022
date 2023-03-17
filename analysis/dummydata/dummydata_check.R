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
  
  if(length(not_in_studydef)!=0) stop(
    paste(
      "These variables are in custom but not in studydef: ",
      paste(not_in_studydef, collapse=", ")
    )
  )
  
  # reorder columns
  dummydata_studydef <- dummydata_studydef[,names(dummydata_custom)]
  
  unmatched_types <- cbind(
    map_chr(dummydata_studydef, ~paste(class(.), collapse=", ")),
    map_chr(dummydata_custom, ~paste(class(.), collapse=", "))
  )[ (map_chr(dummydata_studydef, ~paste(class(.), collapse=", ")) != map_chr(dummydata_custom, ~paste(class(.), collapse=", ")) ), ] %>%
    as.data.frame() %>% rownames_to_column()
  
  if(nrow(unmatched_types)>0) stop(
    #unmatched_types
    "inconsistent typing in studydef : dummy dataset\n",
    apply(unmatched_types, 1, function(row) paste(paste(row, collapse=" : "), "\n"))
  )
  
  print("studydef and custom dummy data match!")
  
}
