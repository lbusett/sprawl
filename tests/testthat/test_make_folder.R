test_that("make_folder works as expected", {
  context("make_folder from a folder name")
  path <- file.path(tempdir(), "testdir")
  dir.exists(path)
  make_folder(path)
  dir.exists(dirname(path))
})

test_that("make_folder works as expected", {
  context("make_folder from a filename")
  path <- file.path(tempdir(), "testdir")
  dir.exists(path)
  make_folder(path)
  dir.exists(dirname(path))
})
