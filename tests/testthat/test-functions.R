


obj1 <- create_cnt_ptable(D = 3, V = 1)
obj_nums <- pt_ex_nums()
obj_nums_odd <- create_num_ptable(
  D = 10,
  V = 2,
  optim = c(4, 1),
  step = 2,
  icat = c(1, 10),
  type = "odd")
obj_nums2 <- pt_ex_nums(FALSE, TRUE)
obj_nums3 <- pt_ex_nums(FALSE, FALSE)
obj_nums4 <- pt_ex_nums(TRUE, TRUE)

test_that("Error capturing", {
  expect_error(fifi_plot(obj1, type="d", file=123))
  
  expect_error(fifi_plot(obj1, type="d", file=c("abc","def")))
  
  expect_error(fifi_plot(obj1, type="u", file="test"))
  
  expect_error(plot(obj1, type="u", file="test"))
  
  #expect_output(fifi_plot(obj1, type="d"), "Distribution of Perturbation Values")
  #expect_output(fifi_plot(obj1, type="p"), "Perturbation Panel")
  #expect_output(fifi_plot(obj1, type="t"), "Transition Matrix")
  
  expect_no_error(fifi_plot(obj1, type="d"))
  expect_no_error(fifi_plot(obj1, type="p"))
  expect_no_error(fifi_plot(obj1, type="t"))
  
  expect_error(pt_export(obj1, SDCtool = "TauArgus")) # lines 22-24
  expect_error(pt_export(obj1, SDCtool = "CK")) # line 27
  expect_error(pt_export(obj1, obj1, obj1, file = "outputfile", SDCtool = "TauArgus")) # 36-37
  
  expect_error(pt_export(100, SDCtool = "TauArgus")) # line 40
  
  expect_error(pt_export(obj1, obj1, file = "outputfile", SDCtool = "TauArgus")) # 51
  expect_error(pt_export(obj_nums_odd, file = "outputfile", SDCtool = "TauArgus")) # 54-59
 
  expect_error(pt_export(obj_nums_odd, obj_nums_odd, file = "outputfile", SDCtool = "TauArgus")) #77-84
  #expect_no_error(pt_export(obj_nums3, file = "outputfile", SDCtool = "TauArgus") )
  
  
  expect_message(pt_export(obj1, file = "outputfile", SDCtool = "TauArgus")) 
  
  expect_no_error(pt_check(obj1))
  expect_no_error(pt_check(obj1@pTable))
})
