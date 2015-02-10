
CreateOrderRecorder <- function() {
  record  <- list();
  
  track <- function(elem1, elem2, relationship) {
    if (! is.list(record[[elem1]])) {record[[elem1]] <<- list()}
    if (! is.list(record[[elem2]])) {record[[elem2]] <<- list()}
    if (relationship == "equals") {        
      record[[elem1]]$equals <<- append(record[[elem1]]$equals, c(elem2))
      record[[elem2]]$equals <<- append(record[[elem2]]$equals, c(elem1))
    } else if (relationship == "larger") {
      record[[elem1]]$inferiors <<- append(record[[elem1]]$inferiors, c(elem2))
      record[[elem2]]$superiors <<- append(record[[elem2]]$superiors, c(elem1))
    } else if (relationship == "smaller") {
      record[[elem1]]$superiors <<- append(record[[elem1]]$superiors, c(elem2))
      record[[elem2]]$inferiors <<- append(record[[elem2]]$inferiors, c(elem1))
    } else if (relationship == "unresolved") {
      record[[elem1]]$unresolved <<- append(record[[elem1]]$equals, c(elem2))
      record[[elem2]]$unresolved <<- append(record[[elem2]]$equals, c(elem1))        
    } else { 
      return (-1);
    }
    return (0)
  }
  
  isCorrect <- function(elem1, elem2, relationship) {
    if (relationship == "equals") {        
      return (elem2 %in% record[[elem1]]$equals && elem1 %in% record[[elem2]]$equals)
    } else if (relationship == "larger") {
      return (elem2 %in% record[[elem1]]$inferiors && elem1 %in% record[[elem2]]$superiors)
    } else if (relationship == "smaller") {
      return (elem2 %in% record[[elem1]]$superiors && elem1 %in% record[[elem2]]$inferiors)
    } else if (relationship == "unresolved") {
      return (elem2 %in% record[[elem1]]$unresolved && elem1 %in% record[[elem2]]$unresolved)
    }
    return (FALSE)
  }
  
  translate <- function(relationship) {
    if (relationship == "superiors") return ("larger")
    if (relationship == "inferiors") return ("smaller")
    return (relationship)
  }
  
  isAllCorrect <- function() {
    for (i in names(record)) {
      for (r in names(record[[i]])) {
        for (j in record[[i]][[r]]) {
          if (i == j) next;
          if (! isCorrect(j, i, translate(r))) {
            message("incorrect", j, i, translate(r))
            return (FALSE)
          }
        }
      }
    }
    return (TRUE)
  }

  orderElems <- function() {
    a <- sapply(record,"[[","superiors")
    elems <- names(a)
    return (elems[order(sapply(a, length))])
  }
  
  equals <- function() {
    sapply(record,"[[","equals")
  }

  
  list(
    track = track,
    isCorrect = isCorrect,
    isAllCorrect = isAllCorrect,
    orderElems = orderElems,
    equals = equals,
    value = function() { return(record) }
  )
}


test <- function() {
  c <- CreateOrderRecorder()
  c$track("s1", "s2", "equals")
  c$track("s1", "s3", "smaller")
  c$track("s1", "s4", "smaller")
  c$track("s2", "s3", "smaller")
  c$track("s2", "s4", "smaller")
  c$track("s3", "s4", "smaller")
  
  return (c)
}
