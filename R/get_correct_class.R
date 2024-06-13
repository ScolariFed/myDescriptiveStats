#' Obtain Class Indication For Every Column Of A Dataframe
#'
#' @param dataframe A dataframe
#' @param unq.max.perc cutoff frequency of the most common variable to distinguish CAT from NUM
#'
#' @return a dataframe with a column of variables and a column of data types
#' @export
#'
#' @examples
#' sample_data.class = get_correct_class(sample_data)

get_correct_class <- function(dataframe,
                              unq.max.perc=0.15){

  #build dataframe with useful parameters for every column
  df=data.frame(
    #name
    name=colnames(dataframe),
    #class - tail is used for ordered columns, which return vector of classes
    class=sapply(dataframe, function(x)tail(class(x),1)),
    #is all of the data NA?
    allNA=sapply(dataframe, function(x)all(is.na(x))),
    #is all of the data an integer?
    allint=sapply(dataframe, function(x){
      if(tail(class(x),1)=="numeric"){
        all(na.omit(floor(x)-x)==0)
      } else NA }),
    #% of unique non-NA categories
    unq_max_perc=sapply(dataframe, function(x){
      if(tail(class(x),1)!="list" & all(is.na(x))==FALSE){
        max(table(x))/nrow(dataframe)
      } else NA })
  )

  #create new column "tipo_dati" based on the data column characteristics
  df <- df |>
    dplyr::mutate(data_type=dplyr::case_when(
      #all NA
      allNA==TRUE~"allNA",
      #character, same value in more than 15% of samples (customizable)
      class=="character" & unq_max_perc>unq.max.perc~"CAT",
      #character, same value in less than 15% of samples (customizable)
      class=="character" & unq_max_perc<=unq.max.perc~"discard",
      #TRUE/FALSE columns or factors are always categorical
      class%in%c("logical","factor")~"CAT",
      #double or numeric,no integer
      (class=="double" | allint==FALSE)~"NUM",
      #integer, same value in more than 15% of samples (customizable)
      (class=="integer" | allint==TRUE) & unq_max_perc>unq.max.perc~"CAT",
      #integer, same value in less than 15% of samples (customizable)
      (class=="integer" | allint==TRUE) & unq_max_perc<=unq.max.perc~"NUM",
      #everything else
      TRUE~"discard"))

  #keep only name of column and new class
  df = df |>
    dplyr::select(name,data_type)

  return(df)
}
