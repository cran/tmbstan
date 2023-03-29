#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
#include "custom_func.hpp"

// Code generated by Stan version 2.21.0

#include <stan/model/model_header.hpp>

namespace model_tmb_namespace {

using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;

static int current_statement_begin__;

stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_tmb");
    reader.add_event(14, 12, "end", "model_tmb");
    return reader;
}

class model_tmb
  : public stan::model::model_base_crtp<model_tmb> {
private:
        int N;
        int have_bounds;
        vector_d lower_bound;
        vector_d upper_bound;
public:
    model_tmb(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, 0, pstream__);
    }

    model_tmb(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, random_seed__, pstream__);
    }

    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        typedef double local_scalar_t__;

        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning

        current_statement_begin__ = -1;

        static const char* function__ = "model_tmb_namespace::model_tmb";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        try {
            // initialize data block variables from context__
            current_statement_begin__ = 2;
            context__.validate_dims("data initialization", "N", "int", context__.to_vec());
            N = int(0);
            vals_i__ = context__.vals_i("N");
            pos__ = 0;
            N = vals_i__[pos__++];
            check_greater_or_equal(function__, "N", N, 1);

            current_statement_begin__ = 3;
            context__.validate_dims("data initialization", "have_bounds", "int", context__.to_vec());
            have_bounds = int(0);
            vals_i__ = context__.vals_i("have_bounds");
            pos__ = 0;
            have_bounds = vals_i__[pos__++];
            check_greater_or_equal(function__, "have_bounds", have_bounds, 0);
            check_less_or_equal(function__, "have_bounds", have_bounds, 1);

            current_statement_begin__ = 4;
            validate_non_negative_index("lower_bound", "(N * have_bounds)", (N * have_bounds));
            context__.validate_dims("data initialization", "lower_bound", "vector_d", context__.to_vec((N * have_bounds)));
            lower_bound = Eigen::Matrix<double, Eigen::Dynamic, 1>((N * have_bounds));
            vals_r__ = context__.vals_r("lower_bound");
            pos__ = 0;
            size_t lower_bound_j_1_max__ = (N * have_bounds);
            for (size_t j_1__ = 0; j_1__ < lower_bound_j_1_max__; ++j_1__) {
                lower_bound(j_1__) = vals_r__[pos__++];
            }

            current_statement_begin__ = 5;
            validate_non_negative_index("upper_bound", "(N * have_bounds)", (N * have_bounds));
            context__.validate_dims("data initialization", "upper_bound", "vector_d", context__.to_vec((N * have_bounds)));
            upper_bound = Eigen::Matrix<double, Eigen::Dynamic, 1>((N * have_bounds));
            vals_r__ = context__.vals_r("upper_bound");
            pos__ = 0;
            size_t upper_bound_j_1_max__ = (N * have_bounds);
            for (size_t j_1__ = 0; j_1__ < upper_bound_j_1_max__; ++j_1__) {
                upper_bound(j_1__) = vals_r__[pos__++];
            }


            // initialize transformed data variables
            // execute transformed data statements

            // validate transformed data

            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 8;
            validate_non_negative_index("y", "N", N);
            num_params_r__ += N;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }

    ~model_tmb() { }


    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        typedef double local_scalar_t__;
        stan::io::writer<double> writer__(params_r__, params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;

        current_statement_begin__ = 8;
        if (!(context__.contains_r("y")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable y missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("y");
        pos__ = 0U;
        validate_non_negative_index("y", "N", N);
        context__.validate_dims("parameter initialization", "y", "vector_d", context__.to_vec(N));
        Eigen::Matrix<double, Eigen::Dynamic, 1> y(N);
        size_t y_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < y_j_1_max__; ++j_1__) {
            y(j_1__) = vals_r__[pos__++];
        }
        try {

// ====== Custom Edit Begin
  if (!have_bounds) {
    writer__.vector_unconstrain(y);
  } else {
  for (int j1__ = 0U; j1__ < N; ++j1__)
    writer__.scalar_lub_unconstrain(lower_bound(j1__), upper_bound(j1__), y(j1__));
  }
  // ====== Custom Edit End

        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable y: ") + e.what()), current_statement_begin__, prog_reader__());
        }

        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }

    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }


    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(std::vector<T__>& params_r__,
                 std::vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {

        typedef T__ local_scalar_t__;

        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // dummy to suppress unused var warning

        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;
        try {
            stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);

            // model parameters
            current_statement_begin__ = 8;
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> y;
            (void) y;  // dummy to suppress unused var warning
            if (jacobian__)

// ====== Custom Edit Begin
  {
  if(!have_bounds) {
    y = in__.vector_constrain(N, lp__);
  } else {
    y.resize(N);
    for (int j1__ = 0U; j1__ < N; ++j1__)
      y(j1__) = in__.scalar_lub_constrain(lower_bound(j1__), upper_bound(j1__), lp__);
  }
  }
  // ====== Custom Edit End

            else

// ====== Custom Edit Begin
  if(!have_bounds) {
  y = in__.vector_constrain(N);
  } else {
  y.resize(N);
  for (int j1__ = 0U; j1__ < N; ++j1__)
    y(j1__) = in__.scalar_lub_constrain(lower_bound(j1__), upper_bound(j1__));
  }
  // ====== Custom Edit End


            // model body

            current_statement_begin__ = 11;

// ====== Custom Edit Begin
lp_accum__.add(custom_func::custom_func(y));
// ====== Custom Edit End


        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        lp_accum__.add(lp__);
        return lp_accum__.sum();

    } // log_prob()

    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }


    void get_param_names(std::vector<std::string>& names__) const {

// ====== Custom Edit Begin
SEXP shortpar_nam = Rf_findVar(Rf_install("shortpar_nam"), custom_func::R_env);
names__ = Rcpp::as<std::vector<std::string> >(shortpar_nam);
return;
// ====== Custom Edit End

        names__.push_back("y");
    }


    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {

// ====== Custom Edit Begin
SEXP shortpar_len = Rf_findVar(Rf_install("shortpar_len"), custom_func::R_env);
for(int i=0; i<LENGTH(shortpar_len); i++) {
  std::vector<size_t> dims__;
  dims__.resize(0);
  int len_i = INTEGER(shortpar_len)[i];
  if (len_i > 1) dims__.push_back(len_i);
  dimss__.push_back(dims__);
}
return;
// ====== Custom Edit End

        std::vector<size_t> dims__;
        dims__.resize(0);
        dims__.push_back(N);
        dimss__.push_back(dims__);
    }

    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        typedef double local_scalar_t__;

        vars__.resize(0);
        stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
        static const char* function__ = "model_tmb_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning

        // read-transform, write parameters

// ====== Custom Edit Begin
Eigen::Matrix<double, Eigen::Dynamic, 1> y;

  {
  if(!have_bounds) {
    y = in__.vector_constrain(N);
  } else {
    y.resize(N);
    for (int j1__ = 0U; j1__ < N; ++j1__)
      y(j1__) = in__.scalar_lub_constrain(lower_bound(j1__), upper_bound(j1__));
  }
  }
  // ====== Custom Edit End

        size_t y_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < y_j_1_max__; ++j_1__) {
            vars__.push_back(y(j_1__));
        }

        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;

        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        if (!include_tparams__ && !include_gqs__) return;

        try {
            if (!include_gqs__ && !include_tparams__) return;
            if (!include_gqs__) return;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }

    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng, params_r_vec, params_i_vec, vars_vec, include_tparams, include_gqs, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }

    std::string model_name() const {
        return "model_tmb";
    }


    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        size_t y_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < y_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "y" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }

        if (!include_gqs__ && !include_tparams__) return;

        if (include_tparams__) {
        }

        if (!include_gqs__) return;
    }


    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        size_t y_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < y_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "y" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }

        if (!include_gqs__ && !include_tparams__) return;

        if (include_tparams__) {
        }

        if (!include_gqs__) return;
    }

}; // model

}  // namespace

typedef model_tmb_namespace::model_tmb stan_model;

#ifndef USING_R

stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}

#endif
