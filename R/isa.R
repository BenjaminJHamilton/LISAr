
#' R6 Class Representing An ISA
#'
#' @description
#' Represent the basic functionality of either a cash ISA or a cash LISA, including
#' government bonuses and penalties.
#'
#'
SavingsAccount <- R6Class(
  "SavingsAccount",
  public = list(
    #' @description
    #' Create a new savings account
    #' @param type Type of account. Either "isa" or "lisa"
    #' @return New `SavingsAccount` object
    initialize = function(type = NA) {
      type <- match.arg(type, c("isa", "lisa"))
      private$.type <- type
    },
    #' @description
    #' Deposit an amount of money in the account.
    #' @param x Money to deposit
    deposit = function(x) {
      if (x < 0) {
        abort(
          message = "Deposits must be greater than 0",
          class = "invalid_deposit"
        )
      }

      # Add in government 25% bonus if the account
      # is a LISA
      if (private$.type == "lisa") {
        x <- x * 1.25
      }

      private$.value <- private$.value + x
      invisible(self)
    },
    #' @description
    #' Withdraw all funds from the account.
    #' @param house_price Price of the house being purchased
    withdraw = function(house_price) {
      final_amount <- private$.value
      private$.value <- 0

      # LISA gets a penalty if withdraw is for house
      # over 450000
      if (private$.type == "lisa" && house_price > 450000) {
        final_amount <- final_amount * 0.75
      }

      return(final_amount)
    },
    #' @description
    #' Add interest to the account.
    #' @param rate Interest rate (AER)
    add_interest = function(rate) {
      private$.value <- private$.value * (1 + x/100)
    },
    #' @description
    #' Get the current value of the account
    get_value = function() {
      private$.value
    },
    #' @description
    #' Overwrite default printing behaviour.
    #' @param ... Unused
    print = function(...) {
      cat("<SavingsAccount> \n")
      cat("  Type:  ", toupper(private$.type), "\n", sep = "")
      cat("  Value: ", round(private$.value, 2), "\n", sep = "")
    }
  ),
  private = list(
    .value = 0,
    .type = NULL
  )
)
