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
                     thetas_Murphy = seq(0, 1, length.out = 101),
                     confidence_level = 0.9) {
  # Add stopping criteria
  if (!is.numeric(confidence_level)) confidence_level <- NA

  # Suppress summarise info
  options(dplyr.summarise.inform = FALSE)


  # Delete y and case_id if supplied
  FC_names <- setdiff(names(df), c("y", "case_id"))

  # Dimensions
  m <- length(FC_names)
  n <- length(df$y)

  # Add a case_id to the individual forecasts in the df
  df <- df %>%
    dplyr::mutate(case_id = 1:nrow(df)) %>%
    dplyr::relocate(case_id, y)

  # Reliability diagram
  RelDiag <- reliabilitydiag::reliabilitydiag(
    dplyr::select(df, !c(y, case_id)),
    y = df$y,
    region.level = confidence_level
  )

  # Recover the PAV-recalibrated forecasts from the cases data frame
  df_PAV <- lapply(RelDiag, function(r) r$cases) %>%
    dplyr::bind_rows(.id = "forecast_name") %>%
    mutate(forecast_name = factor(forecast_name, levels = FC_names)) %>%
    dplyr::select(case_id, y, forecast_name, x, CEP_pav) %>%
    dplyr::arrange(forecast_name, case_id)

  # Calculate the elementary scores with thresholds theta
  # Scale the elementary scores such that the area underneath corresponds to the Brier score
  # We further calculate elementary UNC, MCB  and DSC components
  mean_elem <- function(x, y) {
    function(theta) {
      mean(murphydiagram::extremal_score(
        x = x,
        y = y,
        theta = theta,
        functional = "expectile",
        alpha = 0.5
      ))
    }
  }
  Murphy <- df_PAV %>%
    dplyr::rename(
      forecast = forecast_name,
      forecast_value = x,
      calibrated_forecast_value = CEP_pav
    ) %>%
    dplyr::mutate(forecast_event_freq = mean(y)) %>%
    dplyr::group_by(forecast) %>%
    dplyr::summarize(
      theta = thetas_Murphy,
      elem_score =       4 * sapply(thetas_Murphy, mean_elem(forecast_value, y)),
      elem_score_recal = 4 * sapply(thetas_Murphy, mean_elem(calibrated_forecast_value, y)),
      elem_UNC =         4 * sapply(thetas_Murphy, mean_elem(forecast_event_freq, y)),
      elem_MCB = elem_score - elem_score_recal,
      elem_DSC = elem_UNC - elem_score_recal,
      elem_MCBmDSC = elem_MCB - elem_DSC
    )



  ## ROC data frame: Extract results from the function pROC::roc
  roc <- tibble()
  auc <- tibble()
  for (FC_name in FC_names) {
    df_FC <- filter(df_PAV, forecast_name == FC_name)
    roc_tmp <- list()
    for (choice in c("PAV", "raw")) {
      predictor <- switch(choice,
        PAV = df_FC$CEP_pav,
        raw = df_FC$x
      )
      roc_res <- pROC::roc(df_FC$y, predictor, direction = "<", quiet = TRUE)
      roc_tmp[[choice]] <- tibble::tibble(
        forecast = FC_name,
        specificities = roc_res$specificities,
        sensitivities = roc_res$sensitivities
      )
      auc <- rbind(auc, tibble::tibble(
        forecast = FC_name,
        PAV = ifelse(choice == "PAV", TRUE, FALSE),
        auc = as.numeric(roc_res$auc)
      ))
    }
    roc_tmp$PAV$PAV <- TRUE
    roc <- full_join(roc_tmp$raw, roc_tmp$PAV, by = names(roc_tmp$raw)) %>%
      mutate(PAV = sapply(PAV, isTRUE)) %>%
      rbind(roc, .)
  }
  roc$forecast <- factor(roc$forecast, levels = FC_names)
  auc$forecast <- factor(auc$forecast, levels = FC_names)

  obj <- list(df_PAV = df_PAV, Murphy = Murphy, roc = roc, auc = auc, RelDiag = RelDiag, FC_names = FC_names)
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
