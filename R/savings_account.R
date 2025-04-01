

# savings_account constructor
new_savings_account <- function(x = double(), type = "isa") {

  stopifnot(is.double(x))
  type <- match.arg(type, c("isa", "lisa"))

  new_vctr(x, class = "lisar_savings_account", type = type)

}


savings_account <- function(x = double(), type = "isa") {
  x <- vec_cast(x, double())
  new_savings_account(x, type)
}

account_type <- function(x) attr(x, "type")

#' @export
format.lisar_savings_account <- function(x, ...) {

  out <- formatC(
    vec_data(x),
    format = "f",
    digits = 2,
    big.mark = ","
  )

  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0("£", out[!is.na(x)])
  out

}

#' @export
vec_ptype_abbr.lisar_savings_account <- function(x, ...) {
  "sav_ac"
}

#' @export
vec_ptype_full.lisar_savings_account <- function(x, ...) {
  paste0("savings_account<", account_type(x), ">")
}

# Casting and Coercion

#' @export
vec_ptype2.lisar_savings_account.lisar_savings_account <- function(x, y, ...) {
  if (account_type(x) != account_type(y)) {
    stop("cannot combine different account types")
  }

  new_savings_account(type = account_type(x))
}

#' @export
vec_cast.lisar_savings_account.lisar_savings_account <- function(x, to, ...) {
  new_savings_account(vec_data(x), type = account_type(to))
}

#' @export
vec_ptype2.lisar_savings_account.double <- function(x, y, ...) x
#' @export
vec_ptype2.double.lisar_savings_account <- function(x, y, ...) y

#' @export
vec_cast.lisar_savings_account.double  <- function(x, to, ...) new_savings_account(x, type = account_type(to))
#' @export
vec_cast.double.lisar_savings_account  <- function(x, to, ...) vec_data(x)


#' @export
withdraw_savings <- function(x, ..., house_price = NULL) {
  UseMethod("withdraw_savings")
}


#' @export
withdraw_savings.lisar_savings_account <- function(x, ..., house_price = NULL) {

  if (account_type(x) == "lisa" && (is.null(house_price) || house_price > 450000)) return(vec_data(x) * 0.75)

  vec_data(x)

}
