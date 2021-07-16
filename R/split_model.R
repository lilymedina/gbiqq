split_model(model, data){


  nodes <- model$nodes
  n_nodes <- length(nodes)
  parents <- CausalQueries:::get_parents(model)
  confound <- !is.null(model$confounds_df)
  dag <- model$dag
  if(confound){
    confounds_df <- model$confounds_df
  }
  end_node <-   nodes[n_nodes]
  filter_args <- end_node
  keep_going <- TRUE
  end_nodes_i <-   end_node
  while(keep_going2){
  while (keep_going) {
    filter_args <- c(filter_args, parents[[end_nodes_i]])
    if(confound){
      out <- select_confounds()
      filter_args <- c(filter_args, out$filter_args )
    }
    missing_parents <- unique(c(missing_parents, get_missing_parents()))
    end_nodes_i     <-  missing_parents[length(missing_parents)]
    missing_parents <- missing_parents[1:(length(missing_parents))]
    if(is.null(missing_parents)){
      keep_going <- FALSE

      submodel <- create_submodel()
    }

    # identify the  sub_model$dag that matches with dag and remove those lines
     # from dag...
    # from there get parents and not
    a <- t(apply(dag, 1, function(x) submodel$dag == x))
    b <- c(apply(a, 1, all))
    dag <- dag[!b,]
    nodes <- unique(unlist(dag ))
    parents <- dag$parent
  }


  }




}

select_confounds <- function(model){

  candidates <- NULL
  node_1 <- confounds_df$`node 1`%in% filter_args # also include parents (filter_args?)?
  if(any(node_1))
    candidates <-  confounds_df$`node 2`[node_1]
  node_2 <- confounds_df$`node 2%in% filter_args
  if(any(node_2))
    candidates <- c(candidates, confounds_df$`node 1`[node_2])


   list(confounds_df_i  = confounds_df[node_1 | node_2, ],
        filter_args      = candidates[!candidates %in% filter_args])
}

get_missing_parents(model,
                end_node,
                data){
  parents <- get_parents(model)
  endogenous_vars  <- attr(model, "endogenous_nodes")
  missing_parents <- NULL
  parents_i <-    parents[[end_node]]
  endo_parents <- parents_i%in% endogenous_vars
  if( parents_i%in% endogenous_vars){
    endo_parents <- parents_i[endo_parents]
    include_parents <- apply(X = is.na(data[, endo_parents ]),
                             MARGIN = 2,
                             any)
    missing_parents <- names(include_parents)[include_parents]
  }
  missing_parents
}


create_submodel(model, filter_args){
  dag <- model$dag
  parameters_df <- model$parameters_df
  statement <- model$statement
  nodes <- model$nodes
  nodal_types <- model$nodal_types
  n_nodes <- length(nodes)
  parents <- CausalQueries:::get_parents(model)
  confound <- !is.null(model$confounds_df)
  endogenous_vars  <- attr(model, "endogenous_nodes")
  exogenous_vars <- attr(model, "exogenous_nodes")




  slct <-  dag$children %in% filter_args && dag$parent %in% filter_args
  dag_i <- dag[slct, ]

  parameters_df_i <- parameters_df[parameters_df$node%in%filter_args, ]
  nodes_i <- nodes[nodes %in% filter_args]
  sub_model_i <- list(statement = statement,
                      dag = dag_i,
                      nodes = nodes_i,
                      step ="DAG",
                      nodal_types = nodal_types[nodes_i],
                      parameters_df =   parameters_df_i
  )

  class(sub_model_i) <-'causal_model'

  attr(sub_model_i, "exogenous_nodes")  <- exogenous_vars[exogenous_vars%in%filter_args]
  attr(sub_model_i, "endogenous_nodes") <- endogenous_vars[endogenous_vars%in%filter_args]

  sub_model_i
}
