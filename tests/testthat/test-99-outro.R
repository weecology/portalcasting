context(desc = "cleaning house")

main <- "./testing"
main1 <- "./testing1"
main2 <- "./testing2"

test_that(desc = "placeholder",
          code = {

  expect_equal(1, 1)

})

unlink(file.path(main), recursive = TRUE, force = TRUE)
unlink(file.path(main1), recursive = TRUE, force = TRUE)
unlink(file.path(main2), recursive = TRUE, force = TRUE)

