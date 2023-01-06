# A reprex for trying to understand how data.table works in the context of spatsoc
library(spatsoc)
library(tidyverse)

# A sample dataset
load("repr.Rda")

# convert to a data.table for spatsoc
data.table::setDT(repr)

# Group times
spatsoc::group_times(repr, datetime = "timestamp", threshold = "10 minutes")

# group spatial
spatsoc::group_pts(repr, threshold = 1000, id = "trackId", coords = c("utmE", "utmN"), timegroup = "timegroup")

names(repr) # this works! We have modified `repr` by reference, adding both `timegroup` and `group`

# Okay, so even though it's quite poorly documented in spatsoc, both of these functions have the ability to modify by reference.
# BUT! If we make changes to the data between doing group_times and group_pts, group_pts won't successfully modify by reference.
# A sample dataset
load("repr.Rda")
data.table::setDT(repr)
spatsoc::group_times(repr, datetime = "timestamp", threshold = "10 minutes")
repr2 <- repr %>%
  dplyr::select(-ground_speed)
spatsoc::group_pts(repr2, threshold = 1000, id = "trackId", coords = c("utmE", "utmN"), timegroup = "timegroup")
names(repr2)
# In this case, it doesn't work. The `group` column doesn't get added.

# So, this leads me to ask, what does the dplyr::select() step do to the data that prevents pass-by-reference from working?
class(repr)
class(repr2) # the classes are the same!

str(repr)
str(repr2) # except for the removed ground_speed column (expected), there seems to be no difference!
# aaaaargh what the heck is going on????

# Just to confirm: if we run setDT again on repr2, and *then* try running group_pts, it should work
data.table::setDT(repr2)
spatsoc::group_pts(repr2, threshold = 1000, id = "trackId", coords = c("utmE", "utmN"), timegroup = "timegroup")
names(repr2)
# wait WHAT??? Doing that did not fix the issue.
# So it seems like the dplyr operation has broken this irreversibly and invisibly. I cannot figure out what was changed that is making this not work.
# Goals:
# 1. Figure out what criteria determine whether a data.table object can, or cannot, be modified by reference.
# 2. Figure out how to see those criteria, because clearly class() and str() are not cutting it.
# 3. Figure out how to transform a data.table object from one that can be modified by reference to one that cannot be modified by reference, and vice versa.
# 4. Implement a check in `vultureUtils` to make sure the `group` column is present before any other computations are performed. (Pretty sure this check already exists, but I need to verify.)
# 5. Send a PR to the spatsoc developers for increased documentation of the above, so this doesn't trip up other people.
