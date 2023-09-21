void sa_init(void);
void sa_exit(void);
void sa_anneal_f64(int32_t,
                   double const *, int32_t const *, int32_t const *,
                   double const *,
                   uint32_t,
                   int32_t,
                   int32_t,
                   float, float,
                   uint64_t *,
                   double *);
double sa_compute_energy_f64(int32_t,
                             double const *, int32_t const *, int32_t const *,
                             double const *,
                             uint64_t const *);
void sa_estimate_betas_f64(int32_t,
                           double const *, int32_t const *, int32_t const *,
                           double const *,
                           double *, double *);
double sa_greedy_solve_f64(int32_t,
                           double const *, int32_t const *, int32_t const *,
                           double const *,
                           uint64_t *);
