pipmd_classes <- c("pipmd", "data.table", "data.frame")
pipgd_classes <- c("pipgd", "data.table", "data.frame")

test_that("as_pipmd work", {

  # As data.table
  md <- copy(md_ex)
  as_pipmd(md)
  expect_equal(class(md), pipmd_classes)

  # data.frame
  md <- as.data.frame(md_ex)
  as_pipmd(md)
  expect_equal(class(md), c("data.frame"))


  md <- as_pipmd(md)
  expect_equal(class(md), pipmd_classes)


})


test_that("as_pipgd work", {

  # As data.table
  gd <- copy(gd_ex)
  as_pipgd(gd)
  expect_equal(class(gd), pipgd_classes)

  # data.frame
  gd <- as.data.frame(gd_ex)
  as_pipgd(gd)
  expect_equal(class(gd), c("data.frame"))


  gd <- as_pipgd(gd)
  expect_equal(class(gd), pipgd_classes)


})

test_that("as_pip works with data.frame", {

  # As data.table
  md <- copy(md_ex)

  df <- as_pip(md)
  expect_equal(class(df), class(md))

  # as data.frame
  md <- as.data.frame(md_ex)

  df <- as_pip(md)
  expect_equal(class(md), c("data.frame"))
  expect_equal(class(df), pipmd_classes)

  # As data.table
  gd <- copy(gd_ex)

  df <- as_pip(gd)
  expect_equal(class(df), class(gd))

  # as data.frame
  gd <- as.data.frame(gd_ex)

  df <- as_pip(gd)
  expect_equal(class(gd), c("data.frame"))
  expect_equal(class(df), pipgd_classes)

})

test_that("as_pip work with lists", {

  # As data.table
  md <- copy(md_ex)
  gd <- copy(gd_ex)

  ol <- list(md = md, gd = gd)

  nl <- as_pip(ol)

  expect_equal(class(nl), "list")

  expect_equal(class(nl$md), class(ol$md))
  expect_equal(class(nl$gd), class(ol$gd))


  expect_equal(class(nl$md), pipmd_classes)
  expect_equal(class(nl$gd), pipgd_classes)


  # As data.frame
  md <- as.data.frame(md_ex)
  gd <- as.data.frame(gd_ex)

  ol <- list(md = md, gd = gd)

  nl <- as_pip(ol)

  expect_equal(class(nl), "list")

  expect_equal(class(ol$md), "data.frame")
  expect_equal(class(ol$gd), "data.frame")


  expect_equal(class(nl$md), pipmd_classes)
  expect_equal(class(nl$gd), pipgd_classes)


})
