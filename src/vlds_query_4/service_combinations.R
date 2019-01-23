ones <- transpose(as.data.table(combn(c("snap", "tanf", "foster", "ocs", "vpip"), 1)))
twos <- transpose(as.data.table(combn(c("snap", "tanf", "foster", "ocs", "vpip"), 2)))
threes <- transpose(as.data.table(combn(c("snap", "tanf", "foster", "ocs", "vpip"), 3)))
fours <- transpose(as.data.table(combn(c("snap", "tanf", "foster", "ocs", "vpip"), 4)))
fives <- transpose(as.data.table(combn(c("snap", "tanf", "foster", "ocs", "vpip"), 5)))

all_combos <- 
rbindlist(list(
ones[, .(paste(V1, "only"))],
twos[, .(paste(V1, "&", V2))],
threes[, .(paste(V1, "&", V2, "&", V3))],
fours[, .(paste(V1, "&", V2, "&", V3, "&", V4))],
fives[, .(paste(V1, "&", V2, "&", V3, "&", V4, "&", V5))]
))
