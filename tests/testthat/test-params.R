

#context("Parameter Testing")


para1 <- pt_create_pParams(D = 3, V = 1)


test_that("Error capturing", {
  expect_error(pt_create_pParams(D = 2))
  expect_error(pt_create_pParams(V = 3))
  expect_error(pt_create_pParams(D = 3, V = 1, pstay = 2))
  
  expect_message(pt_create_pParams(D = 3, V = 1, icat = c(1,2)))
  
  expect_error(pt_create_pParams(D = -2, V = 3, js = 2, label = "test"))
  expect_error(pt_create_pParams(D = 2.01, V = 3, js = 2, label = "test"))
  expect_error(pt_create_pParams(D = 2, V = 3, js = -2, label = "test"))
  expect_error(pt_create_pParams(D = 2.01, V = 3, js = 2.01, label = "test"))
  expect_error(pt_create_pParams(D = 2, V = 3, js = 2.01, label = test))
  expect_error(pt_create_pParams(D = 2, V = 3, js = 2.01, label = "test")) 
  
  expect_error(pt_create_pParams(D = 5, V = 3, table = "nums", step = 20, icat = c(1,5), label = "test")) 
  expect_error(pt_create_pParams(D = 5, V = 3, table = "nums", step = -2, icat = c(1,5), label = "test")) 
  expect_message(pt_create_pParams(D = 5, V = 3, table = "nums", step = 2, icat = c(1,5), js = 2, label = "test")) 
  expect_error(pt_create_pParams(D = 5, V = 3, table = "nums", step = 2, icat = c(1,4), label = "test")) 
  expect_error(pt_create_pParams(D = 5, V = 3, table = "nums", step = 2, icat = c(1,5,10), label = "test")) 
  
  expect_error(pt_create_pParams(D = 3, V = 1, pstay = c(0.5,0.5,0.5,0.5,0.5)))
  expect_error(pt_create_pParams(D = 3, V = 1, optim = c(1,1,1,1,1,1,1,1)))
  expect_error(pt_create_pParams(D = 3, V = 1, mono = c(T,T,T,T,T,T,T,T,T,T)))
  
  expect_no_error(pt_create_pTable(params = para1, debugging = TRUE, monitoring = TRUE))
  expect_error(modify_cnt_ptable(create_cnt_ptable(D = 3, V = 1), threshold = -1, seed = 123))
  expect_error(modify_cnt_ptable(create_cnt_ptable(D = 3, V = 1), threshold = 0, seed = 123))
  expect_error(modify_cnt_ptable(create_cnt_ptable(D = 3, V = 1), threshold = 1.1, seed = 123))
  
  expect_error(modify_cnt_ptable(pt_ex_nums(), threshold = 0.1, seed = 123))
  expect_error(modify_cnt_ptable(create_cnt_ptable(D = 3, V = 1)@pTable[, .(i,j,v)] , threshold = 0.1, seed = 123))
  expect_error(modify_cnt_ptable(data.table(i=1, j=2) , threshold = 0.1, seed = 123))
  expect_no_error(modify_cnt_ptable(create_cnt_ptable(D = 3, V = 1), threshold = 0.1, seed = 123))
  expect_no_error(modify_cnt_ptable(create_cnt_ptable(D = 3, V = 1)@pTable, threshold = 0.1, seed = 123))
  
  expect_no_error(modify_cnt_ptable(ck_ptab_cnts, threshold = 0.1, seed = 123))
  expect_error(modify_cnt_ptable(ck_ptab_nums, threshold = 0.1, seed = 123))
  expect_error(modify_cnt_ptable(c(1,2,3), threshold = 0.1, seed = 123))
  expect_message(ptab_mod <- modify_cnt_ptable(pt_create_pTable(para1), 0.3, seed = 5467))
  
  # entropy
  expect_no_error(create_cnt_ptable(D = 3, V = 1, optim = 2))
  expect_no_error(create_cnt_ptable(D = 3, V = 1, optim = 3))
  
  # no create
  expect_no_error(create_cnt_ptable(D = 3, V = 1, optim = 3, create = FALSE))
  
  })
