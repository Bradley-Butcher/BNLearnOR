#'Parse a cpdist query for a given mutilated network, target and evidence node + level
#'
#'@param mutilated.network a fit mutilated network
#'@param target the target to sample in respect to
#'@param node the evidence node
#'@param level the level of the evidence node
#'
#'@return return the probabilities of the target under evidence
#'
#'@import bnlearn
parse.cpdist <- function(mutilated.network, target, node, level) {
  parse.string <- 'parse.result <- prop.table(table(bnlearn::cpdist(mutilated.network, target, ('
  parse.string <- paste0(parse.string, node, ' == \'', level, '\'), batch = 100000)))')
  eval(parse(text = parse.string))
  return(parse.result)
}

#' Produce binary odds ratios 
#'
#' @import bnlearn
Odds.Ratio.Binary <- function(fit, target, evidence.node, reference.level, evidence.level, target.ref, target.target) {
  #Mutiliate the network for reference
  evidence.list <- list(reference.level)
  names(evidence.list) <- c(evidence.node)
  mutilated.network.reference <- bnlearn::mutilated(fit, evidence.list)
  #Calculate distribution of target
  reference.dist <- parse.cpdist(mutilated.network.reference, target, evidence.node, reference.level)
  
  t.idx <- which(names(reference.dist) %in% target.ref)
  r.idx <- which(names(reference.dist) %in% target.target)
  #Mutiliate the network for evidence
  evidence.list <- list(evidence.level)
  names(evidence.list) <- c(evidence.node)
  mutilated.network.evidence <- bnlearn::mutilated(fit, evidence.list)
  evidence.dist <- parse.cpdist(mutilated.network.evidence, target, evidence.node, evidence.level)
  OR <- (evidence.dist[t.idx] / evidence.dist[r.idx]) / (reference.dist[t.idx] / reference.dist[r.idx])
  names(OR) <- c('OR')
  return (if (is.nan(OR) || is.infinite(OR)) 1 else OR)
}

get.levels <- function(fit, target) {
  cmd <- paste0('levels = rownames(fit$', target, '$prob)')
  eval(parse(text = cmd))
  return(levels)
}



#'Produce Odds Ratios plots for a given bnlearn fit object, and configuration file.
#'
#'@param fit the bnlearn fit object
#'@param config the loaded yaml configuration file
#'
#'@import bnlearn
#'@export
#'@examples
#'config <- yaml::read_yaml('config.yml')
#'Produce.OR(fit, config)
Produce.OR <- function(fit, config) {
  
  target <- config$target_variable
  target.reference <- config$target_reference
  
  for(target.target in config$target_levels) {
    Odds.Ratio.Means <- c()
    Odds.Ratio.Lowers <- c()
    Odds.Ratio.Uppers <- c()
    Condition <- c()
    for (node.name in names(config$variable_levels)) {
      
      node.config <- config$variable_levels[[node.name]]
      
      if (target == node.name) {
        next
      }
      
      levels <- node.config[names(node.config) != 'Reference']
      
      reference.level <- levels[1]
      if('Reference' %in% names(node.config))
        reference.level <- node.config['Reference']
      
      evidence.levels <- node.config[node.config != as.character(reference.level)]
      
      for (evidence.level in evidence.levels) {
        ors <- c()
        or.upper <- 0
        or.lower <- 100
        for (i in 1:10) {
          OR <- Odds.Ratio.Binary(fit, target, node.name,
                                  as.character(reference.level),
                                  as.character(evidence.level),
                                  as.character(target.reference),
                                  as.character(target.target))
          ors <- c(ors, OR)
          or.upper <- if (OR > or.upper) OR else or.upper
          or.lower <- if (OR < or.lower) OR else or.lower
        }
        or.mean <- mean(ors)
        Odds.Ratio.Means <- c(Odds.Ratio.Means, or.mean)
        Odds.Ratio.Lowers <- c(Odds.Ratio.Lowers, or.lower)
        Odds.Ratio.Uppers <- c(Odds.Ratio.Uppers, or.upper)
        
        Condition <- c(Condition, paste0(node.name, ' : ', reference.level, '/', evidence.level, ' OR: ', round(or.mean, 3)))
      }
    }
    
    o <- order(Odds.Ratio.Means)
    Condition <- factor(Condition[o], levels = Condition[o])
    Odds.Ratio.Means <- Odds.Ratio.Means[o]
    Odds.Ratio.Lowers <- Odds.Ratio.Lowers[o]
    Odds.Ratio.Uppers <- Odds.Ratio.Uppers[o]
    
    
    df <- data.frame(Condition, Odds.Ratio.Means, Odds.Ratio.Lowers, Odds.Ratio.Uppers)
    draw.forest.plot(df, paste0(target.reference, ' vs. ', target.target))
  }
}


#'@import ggplot2
draw.forest.plot <- function(ORs, title = 'default title') {
  p <- ggplot2::ggplot(ORs, ggplot2::aes(x=Condition, y=Odds.Ratio.Means, ymin=Odds.Ratio.Lowers, ymax=Odds.Ratio.Uppers))+
    ggplot2::geom_hline(yintercept = 1, linetype=2)+
    ggplot2::geom_errorbar(ggplot2::aes(ymin=Odds.Ratio.Lowers, ymax=Odds.Ratio.Uppers), width = 0.3, cex = 0.5)+
    ggplot2::geom_pointrange(color = 'black', fill = 'white') +
    ggplot2::coord_flip()+
    ggplot2::ylim(0, min(c(2, max(ORs$Odds.Ratio.Uppers + 1)))) +
    ggplot2::xlab('Variable: Reference / Evidence') + ggplot2::ylab('Odds Ratio') +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::ggtitle(title)
  print(p)
}

