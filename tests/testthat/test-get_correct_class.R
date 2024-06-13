test_that("get_correct_class() works", {
  expect_equal(get_correct_class(data.frame(x1=rnorm(50,100,10),
                                            x2=rep(c("a","b"),25),
                                            x3=rep(NA,50),
                                            x4=rep(1:5,10),
                                            x5=sample(1:500,50))),
               data.frame(name=c("x1","x2","x3","x4","x5"),
                          data_type=c("NUM","CAT","allNA","CAT","NUM"),
                          row.names = c("x1","x2","x3","x4","x5")))
})
