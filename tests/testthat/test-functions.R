
obj1 <- create_cnt_ptable(D = 3, V = 1)
obj2 <- create_cnt_ptable(D = 7, V = 3)
obj_nums <- pt_ex_nums()
obj_nums_odd <- create_num_ptable(
  D = 10,
  V = 2,
  optim = c(4, 1),
  step = 2,
  icat = c(1, 10),
  type = "odd"
)
obj_nums_even <- create_num_ptable(
  D = 10,
  V = 2,
  optim = c(4, 1),
  step = 2,
  icat = c(1, 10),
  type = "even"
)
obj_nums2 <- pt_ex_nums(FALSE, TRUE)
obj_nums3 <- pt_ex_nums(FALSE, FALSE)
obj_nums4 <- pt_ex_nums(TRUE, TRUE)

test_that("Plot functions are ok", {
  expect_error(fifi_plot(obj1, type = "d", file = 123))
  
  expect_error(fifi_plot(obj1, type = "d", file = c("abc", "def")))
  
  expect_error(fifi_plot(obj1, type = "u", file = "test"))
  
  expect_error(plot(obj1, type = "u", file = "test.pdf"))
  
  expect_error(plot(obj1, type = "d", file = "test.ps"))
  
  expect_no_error(fifi_plot(obj1, type = "d"))
  expect_no_error(fifi_plot(obj1, type = "p"))
  expect_no_error(fifi_plot(obj1, type = "t"))
  
  expect_error(plot(obj1, type = "t", file = 123))
  expect_error(plot(obj1, type = "p", file = 123))
  expect_error(plot(obj1, type = "d", file = 123))
  
  expect_no_error(plot(obj1, type = "t", file = "test.pdf"))
  expect_no_error(plot(obj1, type = "p", file = "test.pdf"))
  expect_no_error(plot(obj1, type = "d", file = "test.pdf"))
  
  expect_no_error(plot(obj2, type = "p"))
})


test_that("Export functions are ok", {
  expect_no_error(pt_export(obj1, 
                            file = "outputfile", 
                            SDCtool = "TauArgus"))
  expect_no_error(pt_export(obj1, 
                            file = "outputfile", 
                            SDCtool = "SAS"))
  expect_no_error(pt_export(obj_nums, 
                            file = "outputfile", 
                            SDCtool = "TauArgus"))
  expect_no_error(pt_export(obj_nums, 
                            file = "outputfile", 
                            SDCtool = "SAS"))
  expect_no_error(pt_export(obj1, 
                            file = "outputfile.txt", 
                            SDCtool = "TauArgus"))
  expect_no_error(pt_export(
    obj_nums_odd,
    obj_nums_even,
    file = "outputfile",
    SDCtool = "TauArgus"
  ))
  expect_no_error(pt_export(
    obj_nums_odd,
    obj_nums_even,
    file = "outputfile",
    SDCtool = "SAS"
  ))

  expect_error(pt_export(obj1,
                         file = "outputfile.",
                         SDCtool = "TauArgus"))
  
  expect_error(pt_export(obj1, SDCtool = "TauArgus")) # lines 22-24
  expect_error(pt_export(obj1, SDCtool = "CK")) # line 27
  expect_error(pt_export(obj1, obj1, obj1, 
                         file = "outputfile", SDCtool = "TauArgus")) # 36-37
  
  expect_error(pt_export(100, SDCtool = "TauArgus")) # line 40
  
  expect_error(pt_export(obj1, obj1, 
                         file = "outputfile", SDCtool = "TauArgus")) # 51
  expect_error(pt_export(obj_nums_odd, 
                         file = "outputfile", SDCtool = "TauArgus")) # 54-59
  
  expect_error(pt_export(
    obj_nums_odd,
    obj_nums_odd,
    file = "outputfile",
    SDCtool = "TauArgus"
  )) #77-84
  
  expect_message(pt_export(obj1, file = "outputfile", SDCtool = "TauArgus"))
})

test_that("Check functions are ok", {
  expect_no_error(pt_check(obj1))
  expect_no_error(pt_check(obj1@pTable))
  
})


test_that("Removed test files", {
  expect_no_error(file.remove("outputfile.txt"))
  expect_no_error(file.remove("test.pdf"))
})
