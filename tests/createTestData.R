

# create test data

ck_ptab_cnts <- cellKey::ck_params_cnts(ptable::pt_ex_cnts())
ck_ptab_nums <- cellKey::ck_params_nums(
  type = "top_contr",
  top_k = 3,
  ptab = ptable::pt_ex_nums(separation = TRUE),
  mult_params = cellKey::ck_flexparams(
    fp = 1000,
    p = c(0.20, 0.03),
    epsilon = c(1, 0.5, 0.2),
    q = 2),
  use_zero_rkeys = TRUE,
  mu_c = 3)

usethis::use_data(ck_ptab_cnts, ck_ptab_nums, internal = TRUE)
