#' @importFrom magrittr %>%
#' @importFrom utils head tail
#' @export
magrittr::`%>%`

#' @export
tidytree <- function(formula, data, ...) {
  data <- model.frame(formula, data = data)
  target <- all.vars(formula[[2]])
  tree <- build_tree(data, target, ...)
  levels <- NULL

  if (is.factor(data[[target]])) {
    levels = levels(data[[target]])
    tree$prediction <- factor(tree$prediction, labels = levels)
  }

  structure(
    list(target = target,
         levels = levels,
         tree = tree),
    class = "tidytree")
}

#' @export
predict.tidytree <- function(model, newdata) {
  predictions <- classify(model$tree, newdata)
  if (!is.null(model$levels)) {
    predictions <- factor(predictions, labels = model$levels)
  }
  predictions
}

#' @export
plot.tidytree <- function(model) {
  plot_tree(model$tree)
}

#' @export
print.tidytree <- function(model) {
  print(glue::glue("tidytree model with {nrow(model$tree)} nodes\n\n"))

  for (nid in model$tree$id) {
    node <- dplyr::slice(model$tree, nid)
    if (node$leaf) {
      print(glue::glue("{paste(rep('  ', node$level), collapse = '')}|-{node$side == 'left'} => {node$prediction}"))
    } else {
      if (node$id == 1) {
        print(glue::glue("({node$feature} < {node$value})"))
      } else {
        print(glue::glue("{paste(rep('  ', node$level), collapse = '')}|-{node$side == 'left'} ({node$feature} < {node$value})"))
      }
    }
  }
}

#' @export
as.data.frame.tidytree <- function(model, ...) {
  as.data.frame(model$tree)
}



classify <- function(model, new_data) {
  cnid <- rep(1L, nrow(new_data))
  pred <- rep(NA, nrow(new_data))

  model_prediction <- model$prediction
  model_leaf <- model$leaf
  model_feature <- model$feature
  model_value <- model$value
  model_left_id <- model$left_id
  model_right_id <- model$right_id

  for (cid in model$id) {

    # which nodes are in the picture at the moment?
    valid <- cnid == cid & is.na(pred)

    if (model_leaf[cid]) {
      # classify
      pred[valid] <- model_prediction[cid]
    } else {
      # move left or right
      cnid[valid] <- dplyr::if_else(new_data[valid, model_feature[cid]] < model_value[cid],
                                    model_left_id[cid], model_right_id[cid])
    }
  }

  pred
}


plot_tree <- function(model) {
  g <- dplyr::select(model, parent, id, dplyr::everything()) %>%
    dplyr::filter(parent > 0) %>%
    igraph::graph_from_data_frame(vertices = model)

  ggraph::ggraph(g, "dendrogram") +
    ggraph::geom_edge_diagonal() +
    ggraph::geom_node_point(ggplot2::aes(filter = leaf, color = prediction), size = 5) +
    ggraph::geom_node_label(ggplot2::aes(filter = !leaf, fill = gini, label = glue::glue("{feature} < {signif(value, 3)}"))) +
    ggraph::geom_node_label(ggplot2::aes(label = size), nudge_y = -(max(model$level))*0.04) +
    ggplot2::scale_fill_gradient(low = "white", high = "red", limits = c(0, 1)) +
    ggplot2::theme_void()
}


gini_impurity <- function(labels) {
  1 - sum((table(labels)  / length(labels))  ^ 2)
}


gini_index <- function(left, right) {
  length_left <- length(left)
  length_right <- length(right)
  total_length <- length_left + length_right
  (1 - sum((table(left)  / length_left)  ^ 2)) * length_left  / total_length +
    (1 - sum((table(right) / length_right) ^ 2)) * length_right / total_length
}


build_tree <- function(df, target,
                       max_level = 5L,
                       id = 1L,
                       parent = 0L,
                       level = 0L,
                       side = NA) {

  # how impure is this data frame?
  gini <- gini_impurity(df[[target]])

  # calculate class probabilities
  probs <-
    df %>%
    dplyr::group_by(.data[[target]]) %>%
    dplyr::summarize(p = n() / nrow(df))

  # determine most probable class
  prediction <-
    probs %>%
    dplyr::arrange(dplyr::desc(p)) %>%
    dplyr::pull(target) %>%
    purrr::pluck(1)

  # information to store for each node in the tree
  this_node <- tibble::data_frame(
    id = id,
    parent = parent,
    level = level,
    side = side,
    leaf = TRUE,
    size = nrow(df),
    feature = NA,
    value = NA,
    left_id = NA,
    right_id = NA,
    gini = gini,
    # probs = list(probs),
    prediction = prediction
  )

  if ((gini > 0) && (level < max_level)) {
    # split because this node is impure
    this_node$leaf <- FALSE

    possible_splits <-
      setdiff(names(df), target) %>%
      purrr::map_df(function(x) {
        v <- unique(sort(df[[x]]))
        list(feature = x, value = list((v[1:length(v) - 1] + v[2:length(v)]) / 2))
      }) %>%
      tidyr::unnest()

    best_split <-
      possible_splits %>%
      dplyr::mutate(
        gini_index = purrr::map2_dbl(
          purrr::map2(feature, value, ~ df[[target]][df[[.x]] <  .y]),
          purrr::map2(feature, value, ~ df[[target]][df[[.x]] >= .y]),
          gini_index)) %>%
      dplyr::arrange(gini_index) %>%
      head(1) %>%
      as.list()

    this_node$feature <- best_split$feature
    this_node$value <- best_split$value

    # split the data according to the best feature, value combination
    left_df  <- df[df[[best_split$feature]] <  best_split$value, ]
    right_df <- df[df[[best_split$feature]] >= best_split$value, ]

    # go left, go right
    this_node$left_id <- id + 1L
    left_child  <- build_tree(left_df,  target, max_level = max_level, id = this_node$left_id,  parent = id, level = level + 1L, side = "left")
    this_node$right_id <- id + nrow(left_child) + 1L
    right_child <- build_tree(right_df, target, max_level = max_level, id = this_node$right_id, parent = id, level = level + 1L, side = "right")

    # combine child nodes with their parent
    this_node <- dplyr::bind_rows(this_node, left_child, right_child)
  }

  this_node
}

