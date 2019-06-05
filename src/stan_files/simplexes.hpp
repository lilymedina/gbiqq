
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.18.1

#include <stan/model/model_header.hpp>

namespace model_simplexes_namespace {

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
    reader.add_event(0, 0, "start", "model_simplexes");
    reader.add_event(71, 69, "end", "model_simplexes");
    return reader;
}

#include <meta_header.hpp>
 class model_simplexes : public prob_grad {
private:
    int n_params;
    int n_types;
    int n_param_sets;
    vector<int> n_param_each;
    int n_data;
    int n_events;
    int n_strategies;
    vector_d lambdas_prior;
    vector<int> l_starts;
    vector<int> l_ends;
    vector<int> strategy_starts;
    vector<int> strategy_ends;
    vector<vector_d> P;
    vector<vector_d> inverted_P;
    matrix_d A;
    matrix_d A_w;
    vector<int> Y;
public:
    model_simplexes(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, 0, pstream__);
    }

    model_simplexes(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
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

        static const char* function__ = "model_simplexes_namespace::model_simplexes";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        // initialize member variables
        try {
            current_statement_begin__ = 3;
            context__.validate_dims("data initialization", "n_params", "int", context__.to_vec());
            n_params = int(0);
            vals_i__ = context__.vals_i("n_params");
            pos__ = 0;
            n_params = vals_i__[pos__++];
            current_statement_begin__ = 4;
            context__.validate_dims("data initialization", "n_types", "int", context__.to_vec());
            n_types = int(0);
            vals_i__ = context__.vals_i("n_types");
            pos__ = 0;
            n_types = vals_i__[pos__++];
            current_statement_begin__ = 5;
            context__.validate_dims("data initialization", "n_param_sets", "int", context__.to_vec());
            n_param_sets = int(0);
            vals_i__ = context__.vals_i("n_param_sets");
            pos__ = 0;
            n_param_sets = vals_i__[pos__++];
            current_statement_begin__ = 6;
            validate_non_negative_index("n_param_each", "n_param_sets", n_param_sets);
            context__.validate_dims("data initialization", "n_param_each", "int", context__.to_vec(n_param_sets));
            validate_non_negative_index("n_param_each", "n_param_sets", n_param_sets);
            n_param_each = std::vector<int>(n_param_sets,int(0));
            vals_i__ = context__.vals_i("n_param_each");
            pos__ = 0;
            size_t n_param_each_limit_0__ = n_param_sets;
            for (size_t i_0__ = 0; i_0__ < n_param_each_limit_0__; ++i_0__) {
                n_param_each[i_0__] = vals_i__[pos__++];
            }
            current_statement_begin__ = 7;
            context__.validate_dims("data initialization", "n_data", "int", context__.to_vec());
            n_data = int(0);
            vals_i__ = context__.vals_i("n_data");
            pos__ = 0;
            n_data = vals_i__[pos__++];
            current_statement_begin__ = 8;
            context__.validate_dims("data initialization", "n_events", "int", context__.to_vec());
            n_events = int(0);
            vals_i__ = context__.vals_i("n_events");
            pos__ = 0;
            n_events = vals_i__[pos__++];
            current_statement_begin__ = 9;
            context__.validate_dims("data initialization", "n_strategies", "int", context__.to_vec());
            n_strategies = int(0);
            vals_i__ = context__.vals_i("n_strategies");
            pos__ = 0;
            n_strategies = vals_i__[pos__++];
            current_statement_begin__ = 11;
            validate_non_negative_index("lambdas_prior", "n_params", n_params);
            context__.validate_dims("data initialization", "lambdas_prior", "vector_d", context__.to_vec(n_params));
            validate_non_negative_index("lambdas_prior", "n_params", n_params);
            lambdas_prior = vector_d(static_cast<Eigen::VectorXd::Index>(n_params));
            vals_r__ = context__.vals_r("lambdas_prior");
            pos__ = 0;
            size_t lambdas_prior_i_vec_lim__ = n_params;
            for (size_t i_vec__ = 0; i_vec__ < lambdas_prior_i_vec_lim__; ++i_vec__) {
                lambdas_prior[i_vec__] = vals_r__[pos__++];
            }
            current_statement_begin__ = 12;
            validate_non_negative_index("l_starts", "n_param_sets", n_param_sets);
            context__.validate_dims("data initialization", "l_starts", "int", context__.to_vec(n_param_sets));
            validate_non_negative_index("l_starts", "n_param_sets", n_param_sets);
            l_starts = std::vector<int>(n_param_sets,int(0));
            vals_i__ = context__.vals_i("l_starts");
            pos__ = 0;
            size_t l_starts_limit_0__ = n_param_sets;
            for (size_t i_0__ = 0; i_0__ < l_starts_limit_0__; ++i_0__) {
                l_starts[i_0__] = vals_i__[pos__++];
            }
            current_statement_begin__ = 13;
            validate_non_negative_index("l_ends", "n_param_sets", n_param_sets);
            context__.validate_dims("data initialization", "l_ends", "int", context__.to_vec(n_param_sets));
            validate_non_negative_index("l_ends", "n_param_sets", n_param_sets);
            l_ends = std::vector<int>(n_param_sets,int(0));
            vals_i__ = context__.vals_i("l_ends");
            pos__ = 0;
            size_t l_ends_limit_0__ = n_param_sets;
            for (size_t i_0__ = 0; i_0__ < l_ends_limit_0__; ++i_0__) {
                l_ends[i_0__] = vals_i__[pos__++];
            }
            current_statement_begin__ = 14;
            validate_non_negative_index("strategy_starts", "n_strategies", n_strategies);
            context__.validate_dims("data initialization", "strategy_starts", "int", context__.to_vec(n_strategies));
            validate_non_negative_index("strategy_starts", "n_strategies", n_strategies);
            strategy_starts = std::vector<int>(n_strategies,int(0));
            vals_i__ = context__.vals_i("strategy_starts");
            pos__ = 0;
            size_t strategy_starts_limit_0__ = n_strategies;
            for (size_t i_0__ = 0; i_0__ < strategy_starts_limit_0__; ++i_0__) {
                strategy_starts[i_0__] = vals_i__[pos__++];
            }
            current_statement_begin__ = 15;
            validate_non_negative_index("strategy_ends", "n_strategies", n_strategies);
            context__.validate_dims("data initialization", "strategy_ends", "int", context__.to_vec(n_strategies));
            validate_non_negative_index("strategy_ends", "n_strategies", n_strategies);
            strategy_ends = std::vector<int>(n_strategies,int(0));
            vals_i__ = context__.vals_i("strategy_ends");
            pos__ = 0;
            size_t strategy_ends_limit_0__ = n_strategies;
            for (size_t i_0__ = 0; i_0__ < strategy_ends_limit_0__; ++i_0__) {
                strategy_ends[i_0__] = vals_i__[pos__++];
            }
            current_statement_begin__ = 17;
            validate_non_negative_index("P", "n_params", n_params);
            validate_non_negative_index("P", "n_types", n_types);
            context__.validate_dims("data initialization", "P", "vector_d", context__.to_vec(n_params,n_types));
            validate_non_negative_index("P", "n_params", n_params);
            validate_non_negative_index("P", "n_types", n_types);
            P = std::vector<vector_d>(n_params,vector_d(static_cast<Eigen::VectorXd::Index>(n_types)));
            vals_r__ = context__.vals_r("P");
            pos__ = 0;
            size_t P_i_vec_lim__ = n_types;
            for (size_t i_vec__ = 0; i_vec__ < P_i_vec_lim__; ++i_vec__) {
                size_t P_limit_0__ = n_params;
                for (size_t i_0__ = 0; i_0__ < P_limit_0__; ++i_0__) {
                    P[i_0__][i_vec__] = vals_r__[pos__++];
            }
            }
            current_statement_begin__ = 18;
            validate_non_negative_index("inverted_P", "n_params", n_params);
            validate_non_negative_index("inverted_P", "n_types", n_types);
            context__.validate_dims("data initialization", "inverted_P", "vector_d", context__.to_vec(n_params,n_types));
            validate_non_negative_index("inverted_P", "n_params", n_params);
            validate_non_negative_index("inverted_P", "n_types", n_types);
            inverted_P = std::vector<vector_d>(n_params,vector_d(static_cast<Eigen::VectorXd::Index>(n_types)));
            vals_r__ = context__.vals_r("inverted_P");
            pos__ = 0;
            size_t inverted_P_i_vec_lim__ = n_types;
            for (size_t i_vec__ = 0; i_vec__ < inverted_P_i_vec_lim__; ++i_vec__) {
                size_t inverted_P_limit_0__ = n_params;
                for (size_t i_0__ = 0; i_0__ < inverted_P_limit_0__; ++i_0__) {
                    inverted_P[i_0__][i_vec__] = vals_r__[pos__++];
            }
            }
            current_statement_begin__ = 19;
            validate_non_negative_index("A", "n_types", n_types);
            validate_non_negative_index("A", "n_data", n_data);
            context__.validate_dims("data initialization", "A", "matrix_d", context__.to_vec(n_types,n_data));
            validate_non_negative_index("A", "n_types", n_types);
            validate_non_negative_index("A", "n_data", n_data);
            A = matrix_d(static_cast<Eigen::VectorXd::Index>(n_types),static_cast<Eigen::VectorXd::Index>(n_data));
            vals_r__ = context__.vals_r("A");
            pos__ = 0;
            size_t A_m_mat_lim__ = n_types;
            size_t A_n_mat_lim__ = n_data;
            for (size_t n_mat__ = 0; n_mat__ < A_n_mat_lim__; ++n_mat__) {
                for (size_t m_mat__ = 0; m_mat__ < A_m_mat_lim__; ++m_mat__) {
                    A(m_mat__,n_mat__) = vals_r__[pos__++];
                }
            }
            current_statement_begin__ = 20;
            validate_non_negative_index("A_w", "n_events", n_events);
            validate_non_negative_index("A_w", "n_data", n_data);
            context__.validate_dims("data initialization", "A_w", "matrix_d", context__.to_vec(n_events,n_data));
            validate_non_negative_index("A_w", "n_events", n_events);
            validate_non_negative_index("A_w", "n_data", n_data);
            A_w = matrix_d(static_cast<Eigen::VectorXd::Index>(n_events),static_cast<Eigen::VectorXd::Index>(n_data));
            vals_r__ = context__.vals_r("A_w");
            pos__ = 0;
            size_t A_w_m_mat_lim__ = n_events;
            size_t A_w_n_mat_lim__ = n_data;
            for (size_t n_mat__ = 0; n_mat__ < A_w_n_mat_lim__; ++n_mat__) {
                for (size_t m_mat__ = 0; m_mat__ < A_w_m_mat_lim__; ++m_mat__) {
                    A_w(m_mat__,n_mat__) = vals_r__[pos__++];
                }
            }
            current_statement_begin__ = 21;
            validate_non_negative_index("Y", "n_events", n_events);
            context__.validate_dims("data initialization", "Y", "int", context__.to_vec(n_events));
            validate_non_negative_index("Y", "n_events", n_events);
            Y = std::vector<int>(n_events,int(0));
            vals_i__ = context__.vals_i("Y");
            pos__ = 0;
            size_t Y_limit_0__ = n_events;
            for (size_t i_0__ = 0; i_0__ < Y_limit_0__; ++i_0__) {
                Y[i_0__] = vals_i__[pos__++];
            }

            // validate, data variables
            current_statement_begin__ = 3;
            check_greater_or_equal(function__,"n_params",n_params,1);
            current_statement_begin__ = 4;
            check_greater_or_equal(function__,"n_types",n_types,1);
            current_statement_begin__ = 5;
            check_greater_or_equal(function__,"n_param_sets",n_param_sets,1);
            current_statement_begin__ = 6;
            for (int k0__ = 0; k0__ < n_param_sets; ++k0__) {
                check_greater_or_equal(function__,"n_param_each[k0__]",n_param_each[k0__],1);
            }
            current_statement_begin__ = 7;
            check_greater_or_equal(function__,"n_data",n_data,1);
            current_statement_begin__ = 8;
            check_greater_or_equal(function__,"n_events",n_events,1);
            current_statement_begin__ = 9;
            check_greater_or_equal(function__,"n_strategies",n_strategies,1);
            current_statement_begin__ = 11;
            check_greater_or_equal(function__,"lambdas_prior",lambdas_prior,0);
            current_statement_begin__ = 12;
            for (int k0__ = 0; k0__ < n_param_sets; ++k0__) {
                check_greater_or_equal(function__,"l_starts[k0__]",l_starts[k0__],1);
            }
            current_statement_begin__ = 13;
            for (int k0__ = 0; k0__ < n_param_sets; ++k0__) {
                check_greater_or_equal(function__,"l_ends[k0__]",l_ends[k0__],1);
            }
            current_statement_begin__ = 14;
            for (int k0__ = 0; k0__ < n_strategies; ++k0__) {
                check_greater_or_equal(function__,"strategy_starts[k0__]",strategy_starts[k0__],1);
            }
            current_statement_begin__ = 15;
            for (int k0__ = 0; k0__ < n_strategies; ++k0__) {
                check_greater_or_equal(function__,"strategy_ends[k0__]",strategy_ends[k0__],1);
            }
            current_statement_begin__ = 17;
            current_statement_begin__ = 18;
            current_statement_begin__ = 19;
            check_greater_or_equal(function__,"A",A,0);
            check_less_or_equal(function__,"A",A,1);
            current_statement_begin__ = 20;
            check_greater_or_equal(function__,"A_w",A_w,0);
            check_less_or_equal(function__,"A_w",A_w,1);
            current_statement_begin__ = 21;
            for (int k0__ = 0; k0__ < n_events; ++k0__) {
                check_greater_or_equal(function__,"Y[k0__]",Y[k0__],0);
            }
            // initialize data variables


            // validate transformed data

            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 26;
            validate_non_negative_index("gamma", "(n_params - n_param_sets)", (n_params - n_param_sets));
            num_params_r__ += (n_params - n_param_sets);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }

    ~model_simplexes() { }


    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        stan::io::writer<double> writer__(params_r__,params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;

        if (!(context__.contains_r("gamma")))
            throw std::runtime_error("variable gamma missing");
        vals_r__ = context__.vals_r("gamma");
        pos__ = 0U;
        validate_non_negative_index("gamma", "(n_params - n_param_sets)", (n_params - n_param_sets));
        context__.validate_dims("initialization", "gamma", "vector_d", context__.to_vec((n_params - n_param_sets)));
        vector_d gamma(static_cast<Eigen::VectorXd::Index>((n_params - n_param_sets)));
        for (int j1__ = 0U; j1__ < (n_params - n_param_sets); ++j1__)
            gamma(j1__) = vals_r__[pos__++];
        try {
            writer__.vector_lb_unconstrain(0,gamma);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable gamma: ") + e.what());
        }

        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }

    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }


    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(vector<T__>& params_r__,
                 vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {

        typedef T__ local_scalar_t__;

        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;

        try {
            // model parameters
            stan::io::reader<local_scalar_t__> in__(params_r__,params_i__);

            Eigen::Matrix<local_scalar_t__,Eigen::Dynamic,1>  gamma;
            (void) gamma;  // dummy to suppress unused var warning
            if (jacobian__)
                gamma = in__.vector_lb_constrain(0,(n_params - n_param_sets),lp__);
            else
                gamma = in__.vector_lb_constrain(0,(n_params - n_param_sets));


            // transformed parameters
            current_statement_begin__ = 30;
            validate_non_negative_index("lambdas", "n_params", n_params);
            Eigen::Matrix<local_scalar_t__,Eigen::Dynamic,1>  lambdas(static_cast<Eigen::VectorXd::Index>(n_params));
            (void) lambdas;  // dummy to suppress unused var warning

            stan::math::initialize(lambdas, DUMMY_VAR__);
            stan::math::fill(lambdas,DUMMY_VAR__);
            current_statement_begin__ = 31;
            validate_non_negative_index("sum_gammas", "n_param_sets", n_param_sets);
            Eigen::Matrix<local_scalar_t__,Eigen::Dynamic,1>  sum_gammas(static_cast<Eigen::VectorXd::Index>(n_param_sets));
            (void) sum_gammas;  // dummy to suppress unused var warning

            stan::math::initialize(sum_gammas, DUMMY_VAR__);
            stan::math::fill(sum_gammas,DUMMY_VAR__);


            current_statement_begin__ = 32;
            for (int i = 1; i <= n_param_sets; ++i) {

                current_statement_begin__ = 34;
                stan::model::assign(sum_gammas, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list()), 
                            (1 + sum(stan::model::rvalue(gamma, stan::model::cons_list(stan::model::index_min_max((get_base1(l_starts,i,"l_starts",1) - (i - 1)), (get_base1(l_ends,i,"l_ends",1) - i)), stan::model::nil_index_list()), "gamma"))), 
                            "assigning variable sum_gammas");
                current_statement_begin__ = 37;
                stan::model::assign(lambdas, 
                            stan::model::cons_list(stan::model::index_min_max(get_base1(l_starts,i,"l_starts",1), get_base1(l_ends,i,"l_ends",1)), stan::model::nil_index_list()), 
                            divide(append_row(1,stan::model::rvalue(gamma, stan::model::cons_list(stan::model::index_min_max((get_base1(l_starts,i,"l_starts",1) - (i - 1)), (get_base1(l_ends,i,"l_ends",1) - i)), stan::model::nil_index_list()), "gamma")),get_base1(sum_gammas,i,"sum_gammas",1)), 
                            "assigning variable lambdas");
            }

            // validate transformed parameters
            for (int i0__ = 0; i0__ < n_params; ++i0__) {
                if (stan::math::is_uninitialized(lambdas(i0__))) {
                    std::stringstream msg__;
                    msg__ << "Undefined transformed parameter: lambdas" << '[' << i0__ << ']';
                    throw std::runtime_error(msg__.str());
                }
            }
            for (int i0__ = 0; i0__ < n_param_sets; ++i0__) {
                if (stan::math::is_uninitialized(sum_gammas(i0__))) {
                    std::stringstream msg__;
                    msg__ << "Undefined transformed parameter: sum_gammas" << '[' << i0__ << ']';
                    throw std::runtime_error(msg__.str());
                }
            }

            const char* function__ = "validate transformed params";
            (void) function__;  // dummy to suppress unused var warning
            current_statement_begin__ = 30;
            check_greater_or_equal(function__,"lambdas",lambdas,0);
            current_statement_begin__ = 31;
            check_greater_or_equal(function__,"sum_gammas",sum_gammas,1);

            // model body
            {
            current_statement_begin__ = 44;
            validate_non_negative_index("w", "n_data", n_data);
            Eigen::Matrix<local_scalar_t__,Eigen::Dynamic,1>  w(static_cast<Eigen::VectorXd::Index>(n_data));
            (void) w;  // dummy to suppress unused var warning

            stan::math::initialize(w, DUMMY_VAR__);
            stan::math::fill(w,DUMMY_VAR__);
            current_statement_begin__ = 45;
            validate_non_negative_index("w_full", "n_events", n_events);
            Eigen::Matrix<local_scalar_t__,Eigen::Dynamic,1>  w_full(static_cast<Eigen::VectorXd::Index>(n_events));
            (void) w_full;  // dummy to suppress unused var warning

            stan::math::initialize(w_full, DUMMY_VAR__);
            stan::math::fill(w_full,DUMMY_VAR__);
            current_statement_begin__ = 46;
            validate_non_negative_index("prob_of_types", "n_types", n_types);
            Eigen::Matrix<local_scalar_t__,Eigen::Dynamic,1>  prob_of_types(static_cast<Eigen::VectorXd::Index>(n_types));
            (void) prob_of_types;  // dummy to suppress unused var warning

            stan::math::initialize(prob_of_types, DUMMY_VAR__);
            stan::math::fill(prob_of_types,DUMMY_VAR__);
            current_statement_begin__ = 47;
            validate_non_negative_index("P_lambdas", "n_params", n_params);
            validate_non_negative_index("P_lambdas", "n_types", n_types);
            vector<Eigen::Matrix<local_scalar_t__,Eigen::Dynamic,1> > P_lambdas(n_types, (Eigen::Matrix<local_scalar_t__,Eigen::Dynamic,1> (static_cast<Eigen::VectorXd::Index>(n_params))));
            stan::math::initialize(P_lambdas, DUMMY_VAR__);
            stan::math::fill(P_lambdas,DUMMY_VAR__);


            current_statement_begin__ = 49;
            for (int i = 1; i <= n_types; ++i) {

                current_statement_begin__ = 50;
                for (int j = 1; j <= n_params; ++j) {

                    current_statement_begin__ = 51;
                    stan::model::assign(P_lambdas, 
                                stan::model::cons_list(stan::model::index_uni(i), stan::model::cons_list(stan::model::index_uni(j), stan::model::nil_index_list())), 
                                ((get_base1(get_base1(P,j,"P",1),i,"P",2) * get_base1(lambdas,j,"lambdas",1)) + get_base1(get_base1(inverted_P,j,"inverted_P",1),i,"inverted_P",2)), 
                                "assigning variable P_lambdas");
                }
                current_statement_begin__ = 53;
                stan::model::assign(prob_of_types, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list()), 
                            prod(get_base1(P_lambdas,i,"P_lambdas",1)), 
                            "assigning variable prob_of_types");
            }
            current_statement_begin__ = 56;
            stan::math::assign(w, multiply(A,prob_of_types));
            current_statement_begin__ = 57;
            stan::math::assign(w_full, multiply(A_w,w));
            current_statement_begin__ = 60;
            lp_accum__.add(gamma_log(lambdas,lambdas_prior,1));
            current_statement_begin__ = 61;
            for (int i = 1; i <= n_param_sets; ++i) {

                current_statement_begin__ = 62;
                lp_accum__.add((-(get_base1(n_param_each,i,"n_param_each",1)) * stan::math::log(get_base1(sum_gammas,i,"sum_gammas",1))));
            }
            current_statement_begin__ = 65;
            for (int i = 1; i <= n_strategies; ++i) {

                current_statement_begin__ = 66;
                lp_accum__.add(multinomial_log(stan::model::rvalue(Y, stan::model::cons_list(stan::model::index_min_max(get_base1(strategy_starts,i,"strategy_starts",1), get_base1(strategy_ends,i,"strategy_ends",1)), stan::model::nil_index_list()), "Y"),stan::model::rvalue(w_full, stan::model::cons_list(stan::model::index_min_max(get_base1(strategy_starts,i,"strategy_starts",1), get_base1(strategy_ends,i,"strategy_ends",1)), stan::model::nil_index_list()), "w_full")));
            }
            }

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
        names__.resize(0);
        names__.push_back("gamma");
        names__.push_back("lambdas");
        names__.push_back("sum_gammas");
    }


    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dims__.push_back((n_params - n_param_sets));
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(n_params);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(n_param_sets);
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
        stan::io::reader<local_scalar_t__> in__(params_r__,params_i__);
        static const char* function__ = "model_simplexes_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        vector_d gamma = in__.vector_lb_constrain(0,(n_params - n_param_sets));
            for (int k_0__ = 0; k_0__ < (n_params - n_param_sets); ++k_0__) {
            vars__.push_back(gamma[k_0__]);
            }

        // declare and define transformed parameters
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;

        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        try {
            current_statement_begin__ = 30;
            validate_non_negative_index("lambdas", "n_params", n_params);
            Eigen::Matrix<local_scalar_t__,Eigen::Dynamic,1>  lambdas(static_cast<Eigen::VectorXd::Index>(n_params));
            (void) lambdas;  // dummy to suppress unused var warning

            stan::math::initialize(lambdas, DUMMY_VAR__);
            stan::math::fill(lambdas,DUMMY_VAR__);
            current_statement_begin__ = 31;
            validate_non_negative_index("sum_gammas", "n_param_sets", n_param_sets);
            Eigen::Matrix<local_scalar_t__,Eigen::Dynamic,1>  sum_gammas(static_cast<Eigen::VectorXd::Index>(n_param_sets));
            (void) sum_gammas;  // dummy to suppress unused var warning

            stan::math::initialize(sum_gammas, DUMMY_VAR__);
            stan::math::fill(sum_gammas,DUMMY_VAR__);


            current_statement_begin__ = 32;
            for (int i = 1; i <= n_param_sets; ++i) {

                current_statement_begin__ = 34;
                stan::model::assign(sum_gammas, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list()), 
                            (1 + sum(stan::model::rvalue(gamma, stan::model::cons_list(stan::model::index_min_max((get_base1(l_starts,i,"l_starts",1) - (i - 1)), (get_base1(l_ends,i,"l_ends",1) - i)), stan::model::nil_index_list()), "gamma"))), 
                            "assigning variable sum_gammas");
                current_statement_begin__ = 37;
                stan::model::assign(lambdas, 
                            stan::model::cons_list(stan::model::index_min_max(get_base1(l_starts,i,"l_starts",1), get_base1(l_ends,i,"l_ends",1)), stan::model::nil_index_list()), 
                            divide(append_row(1,stan::model::rvalue(gamma, stan::model::cons_list(stan::model::index_min_max((get_base1(l_starts,i,"l_starts",1) - (i - 1)), (get_base1(l_ends,i,"l_ends",1) - i)), stan::model::nil_index_list()), "gamma")),get_base1(sum_gammas,i,"sum_gammas",1)), 
                            "assigning variable lambdas");
            }

            // validate transformed parameters
            current_statement_begin__ = 30;
            check_greater_or_equal(function__,"lambdas",lambdas,0);
            current_statement_begin__ = 31;
            check_greater_or_equal(function__,"sum_gammas",sum_gammas,1);

            // write transformed parameters
            if (include_tparams__) {
            for (int k_0__ = 0; k_0__ < n_params; ++k_0__) {
            vars__.push_back(lambdas[k_0__]);
            }
            for (int k_0__ = 0; k_0__ < n_param_sets; ++k_0__) {
            vars__.push_back(sum_gammas[k_0__]);
            }
            }
            if (!include_gqs__) return;
            // declare and define generated quantities



            // validate generated quantities

            // write generated quantities
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
      write_array(base_rng,params_r_vec,params_i_vec,vars_vec,include_tparams,include_gqs,pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }

    static std::string model_name() {
        return "model_simplexes";
    }


    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        for (int k_0__ = 1; k_0__ <= (n_params - n_param_sets); ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "gamma" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }

        if (!include_gqs__ && !include_tparams__) return;

        if (include_tparams__) {
            for (int k_0__ = 1; k_0__ <= n_params; ++k_0__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "lambdas" << '.' << k_0__;
                param_names__.push_back(param_name_stream__.str());
            }
            for (int k_0__ = 1; k_0__ <= n_param_sets; ++k_0__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "sum_gammas" << '.' << k_0__;
                param_names__.push_back(param_name_stream__.str());
            }
        }


        if (!include_gqs__) return;
    }


    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        for (int k_0__ = 1; k_0__ <= (n_params - n_param_sets); ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "gamma" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }

        if (!include_gqs__ && !include_tparams__) return;

        if (include_tparams__) {
            for (int k_0__ = 1; k_0__ <= n_params; ++k_0__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "lambdas" << '.' << k_0__;
                param_names__.push_back(param_name_stream__.str());
            }
            for (int k_0__ = 1; k_0__ <= n_param_sets; ++k_0__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "sum_gammas" << '.' << k_0__;
                param_names__.push_back(param_name_stream__.str());
            }
        }


        if (!include_gqs__) return;
    }

}; // model

}

typedef model_simplexes_namespace::model_simplexes stan_model;


#endif
