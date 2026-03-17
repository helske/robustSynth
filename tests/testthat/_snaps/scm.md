# scm errors on invalid data

    Code
      scm("not a list")
    Condition
      Error in `scm()`:
      ! Argument 'data' should be a list with elements Y1, Y0, X1, X0, Z1, and Z0, or an output from Synth::dataprep.

---

    Code
      scm(list(a = 1))
    Condition
      Error in `scm()`:
      ! Argument 'data' should be a list with elements Y1, Y0, X1, X0, Z1, and Z0, or an output from Synth::dataprep.

# scm errors on invalid trials

    Code
      scm(d, trials = 1.5)
    Condition
      Error in `scm()`:
      ! Argument 'trials' should be positive integer.

# scm errors on invalid alpha

    Code
      scm(d, trials = 1, alpha = -1)
    Condition
      Error in `scm()`:
      ! Argument 'alpha' should be positive numeric scalar.

# scm errors on invalid fixed_v

    Code
      scm(d, fixed_v = c(0.5, 0.5))
    Condition
      Error in `scm()`:
      ! Fixed weight vector `fixed_v` is not a valid simplex.

# scm errors on negative v_zerotol

    Code
      scm(d, trials = 1, qp_control = list(v_zerotol = -1))
    Condition
      Error in `scm()`:
      ! Argument 'v_zerotol' in 'qp_control' should be non-negative scalar.

