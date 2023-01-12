library(tidyverse)
library(patchwork)
library(reliabilitydiag)
library(reshape2)
library(murphydiagram)
library(pROC)

library(patchwork)
library(geomtextpath)
library(ggrepel)
library(grid)


#' Calculations for a Triptych Plot
#'
#' @param df A data.frame containing the forecasts and realizations;
#'  the realizations must have column name "y" and the remaining column names
#'  are used  for the forecasts.
#' @param thetas_Murphy Threshold values for the Murphy diagram;
#'  a sequence of numbers between 0 and 1 that are used for evaluate the Murphy
#'  diagram at.
#' @param confidence_level Confidence level used in the reliability diagram;
#'
#' @return
#' @export
#'
#' @examples
triptych <- function(df,
                     thetas_Murphy = seq(0,1,length.out=101),
                     confidence_level = 0.9){


  # Add stopping criteria
  if (!is.numeric(confidence_level)) confidence_level <- NA

  # Suppress summarise info
  options(dplyr.summarise.inform = FALSE)


  # Delete y and case_id if supplied
  FC_names <- df %>%
    dplyr::select(-intersect(colnames(.), c("y","case_id"))) %>%
    names()

  # Dimensions
  m <- length(FC_names)
  n <- length(df$y)

  # Add a case_id to the individual forecasts in the df
  df <- df %>%
    dplyr::mutate(case_id = 1:nrow(df)) %>%
    dplyr::relocate(case_id,y)

  # Create a long df
  df_long <- reshape2::melt(df,
                            id.vars=c("case_id","y"),
                            variable.name="forecast_name",
                            value.name="forecast_value") %>%
    tibble::as_tibble()


  # Reliability diagram
  RelDiag <- reliabilitydiag::reliabilitydiag(df %>% dplyr::select(-c(y,case_id)),
                                              y = df %>% dplyr::pull(y),
                                              region.level=confidence_level)


  # Extract a data.frame containing all "cases" from the reliabilitydiag option
  cases <- sapply(FC_names, function(name){ RelDiag[[name]]$cases }, simplify = FALSE, USE.NAMES = TRUE) %>%
    dplyr::bind_rows(.id = "forecast")


  # Recover the PAV-recalibrated forecasts from the cases data frame
  df_PAV <- lapply(FC_names,
                   function(name) { RelDiag[[name]]$cases %>%
                       dplyr::select(c(case_id, y, x, CEP_pav)) %>%
                       reshape2::melt(measure.vars=c("x","CEP_pav"), variable.name="PAV", value.name="forecast_value") %>%
                       dplyr::mutate(forecast_name=name, PAV=(PAV=="CEP_pav"),
                                     forecast_name=as.factor(forecast_name)) %>%
                       tibble::as_tibble()
                   }) %>%
    dplyr::bind_rows() %>%
    dplyr::relocate(case_id,y) %>%
    dplyr::arrange(forecast_name, PAV, case_id)



  # Calculate the elementary scores with thresholds theta
  # Scale the elementary scores such that the area underneath corresponds to the Brier score
  # We further calculate elementary UNC, MCB  and DSC components
  Murphy <- df_PAV %>%
    reshape2::dcast(case_id + y + forecast_name ~ PAV, value.var="forecast_value") %>%
    dplyr::rename(forecast=forecast_name, forecast_value='FALSE', calibrated_forecast_value='TRUE') %>%
    dplyr::mutate(forecast_event_freq=mean(y)) %>%
    tibble::as_tibble() %>%
    dplyr::group_by(forecast) %>%
    dplyr::summarize(theta=thetas_Murphy,
              elem_score = 4*sapply(thetas_Murphy, function(theta) mean(murphydiagram::extremal_score(x=forecast_value, y=y, theta=theta, functional = "expectile", alpha = 0.5))),
              elem_score_recal = 4*sapply(thetas_Murphy, function(theta) mean(murphydiagram::extremal_score(x=calibrated_forecast_value, y=y, theta=theta, functional = "expectile", alpha = 0.5))),
              elem_UNC = 4*sapply(thetas_Murphy, function(theta) mean(murphydiagram::extremal_score(x=forecast_event_freq, y=y, theta=theta, functional = "expectile", alpha = 0.5))),
              elem_MCB = elem_score - elem_score_recal,
              elem_DSC = elem_UNC - elem_score_recal,
              elem_MCBmDSC = elem_MCB - elem_DSC)



  ## ROC data frame: Extract results from the function pROC::roc
  roc <- tibble()
  auc <- tibble()
  for (FC_name in FC_names){
    for (PAV.choice in c(TRUE, FALSE)){
      df_FC <- df_PAV %>% dplyr::filter(PAV==PAV.choice & forecast_name==FC_name)
      roc_res <- pROC::roc(response = df_FC %>% dplyr::pull(y),
                           predictor = df_FC %>% dplyr::pull(forecast_value),
                           quiet=TRUE)
      roc <- rbind(roc, tibble::tibble(forecast=FC_name, PAV=PAV.choice, specificities=roc_res$specificities, sensitivities=roc_res$sensitivities))
      auc <- rbind(auc, tibble::tibble(forecast=FC_name, PAV=PAV.choice, auc=roc_res$auc%>%as.numeric()))
    }
  }
  roc$forecast <- factor(roc$forecast, levels=FC_names)
  auc$forecast <- factor(auc$forecast, levels=FC_names)

  obj <- list(df_PAV=df_PAV, Murphy=Murphy, roc=roc, auc=auc, RelDiag=RelDiag, FC_names=FC_names)
  class(obj) <- "triptych"

  obj
}



#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is.triptych <- function(x) {
  inherits(x, "triptych")
}






