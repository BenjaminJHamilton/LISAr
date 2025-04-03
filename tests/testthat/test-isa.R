


test_that("Deposit functionality is correct", {

  account <- SavingsAccount$new(type = "isa")

  expect_equal(account$get_value(), 0)

  account$deposit(500)
  expect_equal(account$get_value(), 500)

  account$deposit(1000)
  expect_equal(account$get_value(), 1500)

  expect_error(account$deposit(-1000), class = "invalid_deposit")

})
