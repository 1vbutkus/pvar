

context("Front C functions")


test_that("pvarC:Exeptions", {
  expect_that( pvarC(0, 2, 1),  not(throws_error())) # OK
  expect_that( pvarC(0, 1, 1),  throws_error())
  expect_that( pvarC(0, 2, 0),  throws_error())
  expect_that( pvarC(0, 2, -1),  throws_error())
  expect_that( pvarC(vector("numeric", 0), 2, -1),  throws_error())
  expect_that( pvarC(c(1, NA), 2, -1),  throws_error())
  expect_that( pvarC(0, NA, 1),  throws_error())
  expect_that( pvarC(0, 2, NA),  throws_error())

})

test_that("AddPvarC:random", {
  test_AddPvar <- function(x, p){
    N = length(x)
    n1 = sample(N, 1);
    n2 = sample(N-1, 1);
    all(
      identical(pvarC(as.vector(x), p), AddPvarC(pvarC(x[1:n1], p), pvarC(x[(n1):N], p), AddIfPossible=TRUE))
      ,identical(pvarC(as.vector(x), p), AddPvarC(pvarC(x[1:n2], p), pvarC(x[(n2+1):N], p), AddIfPossible=FALSE))
    )
  }
  expect_that(all(replicate(20, test_AddPvar(rwiener(50), 2))), is_true())
  expect_that(all(replicate(20, test_AddPvar(rbridge(50), 2))), is_true())
  expect_that(all(replicate(200, test_AddPvar(rcumbin(50), 2))), is_true())
})

test_that("ChangePoints:Exeptions", {
  expect_that( ChangePoints(1),  not(throws_error())) # OK
  expect_that( ChangePoints(vector("numeric", 0)),  throws_error())
  expect_that( ChangePoints(NA),  throws_error())
})


test_that("ChangePoints:random", {
  test_ChangePoints <- function(x){
    SafeRoundCompare(ChangePointsId_R(x), ChangePoints(x))
  }
  expect_that(all(replicate(20, test_ChangePoints(rwiener(50)))), is_true())
  expect_that(all(replicate(20, test_ChangePoints(rbridge(50)))), is_true())
  expect_that(all(replicate(20, test_ChangePoints(rcumbin(50)))), is_true())
  expect_that(all(replicate(200, test_ChangePoints(rcumbin(10)))), is_true())
})




