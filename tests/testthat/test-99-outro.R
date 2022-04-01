context(desc = "cleaning house")

main <- "./testing"

test_that(desc = "placeholder",
          code = {

  expect_equal(1, 1)

})

unlink(main_path(main = main), recursive = TRUE, force = TRUE)

