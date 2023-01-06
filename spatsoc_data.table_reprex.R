# A reprex for trying to understand how data.table works in the context of spatsoc
library(spatsoc)
library(tidyverse)

# A sample dataset
load("repr.Rda") # some sample data, just 25 rows of my GPS observations
class(repr) # this is a data.frame. It's not a data.table.

# convert to a data.table for spatsoc
data.table::setDT(repr) # apparently this is what you're supposed to do in order to turn a data.frame into a data.table so that data.table functions can be used on it.

# Group times
spatsoc::group_times(repr, datetime = "timestamp", threshold = "10 minutes") # expected output: a "timegroup" column gets added by reference. I did not need to reassign, e.g. repr <- spatsoc::group_times(repr...)

# group spatial
spatsoc::group_pts(repr, threshold = 1000, id = "trackId", coords = c("utmE", "utmN"), timegroup = "timegroup") # expected output: a "group" column gets added by reference. Once again, I did not need to reassign.

names(repr) # this works! We have modified `repr` by reference, adding both `timegroup` and `group` columns.

# Okay, so even though it's quite poorly documented in spatsoc, both of these functions seem to have the ability to modify by reference. Note that it *is* documented in spatsoc, but only here (https://docs.ropensci.org/spatsoc/articles/faq.html), not in the actual help files for group_times and group_pts. Grrr.

# HOWEVER! There is a problem.
# If we make changes to the data between doing group_times and group_pts, group_pts will not successfully modify by reference.

# Here's an example (same code as above except for removing a column)
# A sample dataset
load("repr.Rda") # same as above
data.table::setDT(repr) # same as above
spatsoc::group_times(repr, datetime = "timestamp", threshold = "10 minutes") # same as above
# remove the ground_speed column, using dplyr/tidyverse.
repr2 <- repr %>%
  dplyr::select(-ground_speed)
# now try to group_pts
spatsoc::group_pts(repr2, threshold = 1000, id = "trackId", coords = c("utmE", "utmN"), timegroup = "timegroup") # expected output: a group column gets added.
names(repr2)
# In this case, it doesn't work. The `group` column doesn't get added.

# So, this leads me to ask, what does the dplyr::select() step do to the data that prevents pass-by-reference from working?
class(repr)
class(repr2) # the classes are the same! i.e. it is still a data.table.

str(repr)
str(repr2) # except for the removed ground_speed column (expected), there seems to be no difference in structure of these two objects!
# aaaaargh what the heck is going on????

# Maybe if we run setDT again on repr2, and *then* try running group_pts, it will work again?
data.table::setDT(repr2)
spatsoc::group_pts(repr2, threshold = 1000, id = "trackId", coords = c("utmE", "utmN"), timegroup = "timegroup")
names(repr2)
# Wait WHAT??? Doing that did not fix the issue. I guess that's because setDT modifies the class to data.table, and since repr2 was already a data.table, nothing changed.
# So it seems like the dplyr operation has broken this irreversibly and invisibly. I cannot figure out what was changed that is making this not work.
# Goals:
# 1. Figure out what criteria determine whether a data.table object can, or cannot, be modified by reference.
# 2. Figure out how to see those criteria, because clearly class() and str() are not cutting it.
# 3. Figure out how to transform a data.table object from one that can be modified by reference to one that cannot be modified by reference, and vice versa.
# 4. Implement a check in `vultureUtils` to make sure the `group` column is present before any other computations are performed. (Pretty sure this check already exists, but I need to verify.)
# 5. Send a PR to the spatsoc developers for increased documentation of the above, so this doesn't trip up other people.
