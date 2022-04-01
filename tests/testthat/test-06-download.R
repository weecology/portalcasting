context(desc = "content downloading functions")

# given the directory was created in test-03

main <- "./testing"



test_that(desc = "download_archive downloads the archive", {


expect_equal(download_archive(main), NULL)

})


