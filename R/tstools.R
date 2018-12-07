# tstools.R -------------------------------------------------------------------------

# Header
# Filename:       tstools.R
# Description:    Contains utility functions for process modelling.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     06 July 2018
# Last Revision:  16 October 2018
# Version:        0.0.2
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     06 July 2018      Initial issue
# 0.0.2     16 October 2018   Function buildProcessMapPackage() modified: argument linkLabel added

buildProcessMapPackage = function(obj, nodeSize = 'totalEntryFreq', nodeColor = 'totalEntryFreq', linkColor = 'totalFreq', linkWidth = 'totalFreq', linkLabel = NULL, linkLength = NULL, config = NULL){
  # todo: build tooltip, link label
  obj %>% verify('list', names_identical = c('nodes', 'links'), null_allowed = F)

  nodes = obj$nodes
  links = obj$links

  # if(is.empty(nodes)){return(NULL)}
  if(!inherits(nodeSize, c('numeric', 'integer'))){
    nodeSize  %<>% verify('character', lengths = 1, domain = numerics(nodes), default = 100)
  }

  nodeColor %<>% verify('character', lengths = 1, default = 'lightblue')
  if(nodeColor %in% names(nodes)){nodes %<>% mutate_(color = nodeColor)} else {nodes %<>% mutate(color = nodeColor)}

  linkColor %<>% verify('character', lengths = 1, default = 'blue')
  if(linkColor %in% names(links)){links %<>% mutate_(color = linkColor)} else {links %<>% mutate(color = linkColor)}

  nodes %<>% mutate_(size = nodeSize, tooltip = 'status', label = 'status') %>%
    mutate(shape = ifelse(status %in% c('CASE START', 'CASE END'), 'circle', ifelse(status %in% c('ENTER', 'EXIT'), 'diamond', 'rectangle'))) %>%
    select(ID = status, label, size, shape, color, tooltip) %>% na2zero

  if(nodeSize == 'totalEntryFreq'){nodes$size[nodes$label == 'CASE START'] <- nodes$size[nodes$label == 'CASE END'] %>% sum}

  if(!inherits(linkWidth, c('numeric', 'integer'))){
    linkWidth  %<>% verify('character', lengths = 1, domain = numerics(links), default = 40)
  }

  if(!inherits(linkLength, c('numeric', 'integer'))){
    linkLength  %<>% verify('character', lengths = 1, domain = numerics(links), default = 40)
  }

  links %<>% mutate_(width = linkWidth, length = linkLength, label = linkLabel) %>%
    select(source = status, target = nextStatus, color, length, width, label) %>% na2zero

  cfg = config %<==>%
    list(link.width.max = 5, link.width.min = 1, link.smooth = list(enabled = T, type = 'curvedCCW'),
         node.label.size = 25, node.physics.enabled = T, layout = 'hierarchical', direction = 'up.down')

  return(list(data = list(nodes = nodes, links = links), config = cfg))
}

buildTreeTable = function(traces){
  bindrow = function(df, rn, row){
    df[rn, sequence(length(row))] <- row
    return(df)
  }
  aa = traces %>% pull(path) %>% strsplit('-')
  df = data.frame.na(nrow = length(aa), ncol = aa %>% sapply(length) %>% max)
  for (i in sequence(length(aa))){
    df %<>% bindrow(i, aa[[i]])
  }
  colnames(df) <- 'Step' %>% paste(sequence(ncol(df)), sep = '.')
  rownames(df) <- traces$variation
  df$freq      <- traces$freq
  return(df)
}




