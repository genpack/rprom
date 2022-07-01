# Header
# Filename:      promvis.R
# Description:   This module provides functions for visualising a process.
# Author:        Nima Ramezani
# Email :        nima.ramezani@gmail.com
# Start Date:    04 May 2018
# Last Revision: 06 June 2018
# Version:       0.0.3

# Version   Date               Action
# -----------------------------------
# 0.0.1     04 May 2018        Initial issue
# 0.0.2     06 June 2018       Functions addDiagrammeRGraph() modified: Warns if graph tables are not built
# 0.0.3     06 June 2018       Functions plot.process() modified: Warns if plotter = 'DiagrammeR' and no DiagrammeR graph is built.
# -----------------------------------


# addDiagrammeRGraph = function(obj){
#   if(inherits(obj$nodes, c('data.frame', 'tibble', 'data.table')) & inherits(obj$links, c('data.frame', 'tibble', 'data.table'))){
#     nodes = obj$nodes
#     edges = obj$links
#     support('dplyr', 'DiagrammeR')
#     nodes %<>%
#       mutate(weight = scales::rescale(weight, from = c(0, max(weight)))) %>%
#       mutate(weight = ifelse(skill == 'End', Inf, weight))
#
#     if(is.null(obj$DiagrammeRGraph)){
#       create_node_df(n = nrow(nodes),
#                      label = nodes$label,
#                      shape = nodes$shape,
#                      color_level = nodes$weight,
#                      style = "rounded,filled",
#                      fontcolor = nodes$fontcolor,
#                      color = nodes$color,
#                      tooltip = nodes$tooltip,
#                      penwidth = 1.5,
#                      fixedsize = FALSE,
#                      fontname = "Arial") -> nodes_df
#
#       min_level <- min(nodes_df$color_level)
#       max_level <- max(nodes_df$color_level[nodes_df$color_level < Inf])
#
#       create_edge_df(from = edges$from_id,
#                      to = edges$to_id,
#                      label = edges$weight,
#                      penwidth = edges$width,
#                      color = chif(obj$settings$link.weight != "frequency", "red4", "dodgerblue4"),
#                      fontname = "Arial") -> edges_df
#
#       create_graph(nodes_df, edges_df) %>%
#         add_global_graph_attrs(attr = "rankdir", value = "TB",attr_type = "graph") %>%
#         add_global_graph_attrs(attr = "layout", value = "dot", attr_type = "graph") %>%
#         colorize_node_attrs(node_attr_from = "color_level",
#                             node_attr_to = "fillcolor",
#                             palette = chif(obj$settings$link.weight != "frequency", "Reds", "PuBu"),
#                             default_color = "white",
#                             cut_points = seq(min_level-0.1, max_level+.1, length.out = 9)) -> obj$DiagrammeRGraph
#     }} else {
#     warnif(T, "from function addDiagrammeRGraph(): Graph tables are not built! Please add using function 'addGraphTables()' (No graph generated).")
#   }
#   return(obj)
# }
#
#
# plot.process = function(obj, plotter = 'grviz', direction = 'top.down', node_colors = 'navy', edge_colors = 'black', onClickShinyInput = NULL){
#   if(inherits(obj$nodes, c('data.frame', 'tibble', 'data.table')) & inherits(obj$links, c('data.frame', 'tibble', 'data.table'))){
#     cfg = list(point.size = 10, minLinkWidth = 1, maxLinkWidth = 5, shinyInput.click = onClickShinyInput,
#                palette = list(color = c('white', node_colors), linkColor = c('gray', edge_colors)),
#                direction = direction)
#     cat('\n', 'Started plotting the map ...')
#     net = list(nodes = obj$nodes %>% as.data.frame %>% column2Rownames('from_id'), links = obj$links) %>%
#       rvisPlot(label = 'label', shape = 'shape', color = 'weight', tooltip = 'tooltip',
#                source = 'from_id', target = 'to_id', linkLabel = 'weight', linkColor = list(edgeColor = 'weight'),
#                linkWidth = list(edgeWidth = 'weight'),
#                plotter = plotter, type = 'graph', config = cfg)
#
#     cat('Done!', '\n')
#     return(net)
#   }
#   else {
#     warnif(T, "from function plot.process(): Graph tables are not built! Please add using function 'addGraphTables()' (No graph generated).")
#   }
#
#
#   # if(plotter == 'DiagrammeR'){
#   #   obj %<>% addDiagrammeRGraph
#   #   if(is.null(obj$DiagrammeRGraph)){
#   #     warnif(T, "from function plot.process(): Function addDiagrammeRGraph() did not generate any DiagrammeR graph (NULL returned).")
#   #   } else {
#   #     return(obj$DiagrammeRGraph %>% renderDiagrammeRGraph)
#   #     # return(obj$DiagrammeRGraph %>% render_graph)
#   #     # return(obj$DiagrammeRGraph)
#   #   }
#   # } else if (plotter == 'visNetwork'){
#   #   nodes = obj$nodes
#   #   edges = obj$links
#   #
#   #   nodes %<>%
#   #     mutate(weight = scales::rescale(weight, from = c(0, max(weight)))) %>%
#   #     mutate(weight = ifelse(skill == 'End', Inf, weight))
#   #
#   #
#   #   net = list(nodes = obj$nodes %>% as.data.frame %>% column2Rownames('from_id'), links = obj$links) %>%
#   #     rvisPlot(label = 'label', shape = 'shape', color = 'weight', tooltip = 'tooltip', size = 50,
#   #              source = 'from_id', target = 'to_id', linkLabel = 'weight', linkColor = 'weight',
#   #              config = list(point.size = 10, palette = list(color = c('white', node_colors), linkColor = c('white', edge_colors)), direction = direction),
#   #              plotter = 'visNetwork', type = 'graph')
#   #
#   #   return(net %>% visHierarchicalLayout(sortMethod = 'directed', levelSeparation = 200, edgeMinimization = T, blockShifting = T, parentCentralization = T, direction = 'UD') %>% visNodes(physics = F) %>% visEdges(smooth = T))
#   # }
#
#   # tbls = obj %>% graphTables %>%
#   #   rvisPlot(label = 'label', shape = 'shape', color = 'color', labelColor = 'fontcolor', tooltip = 'tooltip',
#   #            linkSource = 'from_id', linkTarget = 'to_id', linkWidth = 'penwidth',
#   #            config = list(point.border.width = 1.5, label.font = 'Arial', node.style = "rounded,filled",
#   #                          link.color = ifelse(perspective_edges == "performance", "red4", "dodgerblue4"),
#   #                          linkLabel.font = "Arial"))
# }
#
# plot.caseIdleTime = function(obj, caseIDs = NULL, unit = "hours", nbars = NULL, descending = T,
#                              plotter = 'plotly', type = 'bar', horizontal = T,
#                              title = 'Total case process idle time' %>% paste0(' (', unit, ')'), config = NULL, ...){
#   if(plotter == 'graphics'){obj$bupaobj %>% idle_time("case", units = unit) %>% plot}
#   else {
#     if (is.null(cases)){cases = unique(obj$cases)}
#     obj$bupaobj %>% idle_time("case", units = unit) %>% filter(caseID %in% cases) -> IDL
#     if(descending){IDL %<>% arrange(desc(idle_time))} else {IDL %<>% arrange(idle_time)}
#     nbars %<>% verify(c('numeric', 'integer'), domain = c(0, nrow(IDL)), default = nrow(IDL), fix = T)
#     IDL = IDL[sequence(nbars), ]
#     cfg = config %<==>% list(title = title, xAxis.label = unit, yAxis.label = 'Case ID',
#                              yAxis.margin.left = 10*(IDL$caseID %>% nchar %>% max(na.rm = T)))
#     IDL %>% rvisPlot(
#       x = chif(horizontal, 'idle_time', 'caseID'),
#       y = chif(horizontal, 'caseID', 'idle_time'),
#       type = type, plotter = plotter, config = cfg, ...)
#   }
# }
#
# plot.skillProcTime = function(obj, measure = 'Average', skills = NULL, unit = 'hours', nbars = NULL, descending = T,
#                               plotter = 'plotly', type = 'bar', horizontal = T,
#                               title = measure %>% paste('skill processing time') %>% paste0(' (', unit, ')'), config = NULL, ...){
#
#   measures = c(Minimum = 'min', 'First quartile' = 'q1', Average = 'mean', Mean = 'mean', Median = 'median', 'Second quartile' = 'median', 'Third quartile' = 'q3', Maximum = 'max', 'Standard deviation' = 'st_dev', 'Total' = 'total')
#   if (is.null(skills)){skills = unique(obj$skills)}
#   obj$bupaobj %>% processing_time("activity", units = unit) %>% filter(taskType %in% skills) %>% as.data.frame -> SPT
#
#   ord = SPT[,measures[measure]] %>% as.numeric %>% order(decreasing = descending)
#   nr = nrow(SPT)
#   nbars %<>% verify(c('numeric', 'integer'), domain = c(0, nr), default = nr, fix = T)
#   SPT = SPT[ord[nbars %>% sequence],]
#   SPT$taskType %<>% as.character
#
#   cfg = config %<==>% list(title = title, xAxis.label = unit, yAxis.label = 'Skill (Task Type)',
#                            yAxis.margin.left = 10*(SPT$taskType %>% nchar %>% max(na.rm = T)))
#
#   SPT %>% rvisPlot(
#     x = chif(horizontal, measures[measure], 'taskType'),
#     y = chif(horizontal, 'taskType', measures[measure]),
#     type = type, plotter = plotter, config = cfg, ...)
# }

