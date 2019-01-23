context("geom_rug")

n = 10
df <- data_frame(x = 1:n, y = (1:n)^3)
p <- ggplot(df, aes(x, y)) + geom_point() + geom_rug(sides = 'l')

test_that("coord_flip flips the rugs", {
  a <- layer_grob(p, 2)
  b <- layer_grob(p + coord_flip(), 2)

  # Rugs along y-axis, all x coordinates are the same
  expect_equal(length(a[[1]]$children[[1]]$x0), 1)
  expect_equal(length(a[[1]]$children[[1]]$x1), 1)
  expect_equal(length(a[[1]]$children[[1]]$y0), n)
  expect_equal(length(a[[1]]$children[[1]]$y1), n)

  # Rugs along x-axis, all y coordinates are the same
  expect_equal(length(b[[1]]$children[[1]]$x0), n)
  expect_equal(length(b[[1]]$children[[1]]$x1), n)
  expect_equal(length(b[[1]]$children[[1]]$y0), 1)
  expect_equal(length(b[[1]]$children[[1]]$y1), 1)
})

df <- tibble::tibble(x = 1:5, y = 1:5)

gg_rug <- ggplot(df, aes(x = x, y = y)) +
  geom_rug()

test_that("geom_rug works", {
  vdiffr::expect_doppelganger("geom_rug", gg_rug)
})

context("geom_rug works with outside option")

df <- tibble::tibble(x = 1:5, y = 1:5)

gg_rug_outside <- ggplot(df, aes(x = x, y = y)) +
  geom_rug(outside = TRUE,
           sides = "tr") +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))

test_that("geom_rug works with outside = TRUE", {
  vdiffr::expect_doppelganger("geom_rug_outside", gg_rug_outside)
})
