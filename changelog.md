# Version history

### 2018-02-19 16:08Z 0.01
* First Shiny version released.

### 2018-02-19 17:23Z 0.02
  * Improved the look of the X-axis (numbers can go sideways if they start to bunch up).
  * Added an upper limit for the X-axis for when no items get close to the scale maximum.
  * Added code to use human-friendly intervals on the Y-axis.

### 2018-02-19 18:31Z 0.03
  * Fixed a bug that caused a previous error/warning message to hang around on the next run.
  * Added version number display

### 2018-02-19 21:43Z 0.04
  * Added input elements to allow a specific response value to appear a fixed number of times.

### 2018-02-20 17:21Z 0.05

  * Fixed a bug that caused a crash 50% of the time when scaleMin and scaleMax were both negative.
  * Added dynamic updates of upper/lower bounds of input controls, depending on the values of others.

### 2018-02-21 15:07Z 0.06
  * Fixed a bug that caused spurious error messages with certain fixed-value configurations.
  * Plots now appear sorted from smallest to largest skewness.
  * Added a rudimentary help feature.
  * Fixed a bug that meant that the user couldn't easily type in a SD close to the permitted minimum.
  * Fixed a bug that could cause errors in extreme cases with delta=2 in rSprite.delta.
  * Improved performance by taking a pragmatic decision about when to stop looking for duplicates.

### 2018-02-21 23:22Z 0.07
  * Added link to enable user to download the data that went into the plots.
  * Fixed a bug that was preventing solutions from being found with very large means and very small SDs.

### 2018-03-03 23:46Z 0.08
  * Increased the size of the plot area.
  * Increased maximum grid size to 10 x 10.
  * Changed plot bar colour for better visibility if black text encroaches on bars.
  * Reduced the chances of missing a valid solution when only a few (requested number > n > 1) exist.
  * Changed displayed name to rSprite.

### 2018-03-24 15:00Z 0.09
  * Display solutions on the smallest grid that they will fit onto.
  * User now chooses the number of results they want, not the grid size.
  * Moved the decimal places input field to just below the mean and SD.
  * Fixed a bug that could cause spurious solutions to be returned if none were possible.

### 2018-03-27 20:23Z 0.10
  * Fixed a bug that could cause the "No solutions found" message to be split into two.
  * Fixed a bug that prevented entering 0 or a negative number as the fixed value.
  * Fixed a bug that prevented a solution from being found in some extreme circumstances.
  * Fixed a bug that produced variable bar widths with large X-axis ranges.

### 2018-04-18 13:50Z 0.11
  * Fixed a bug that prevented the SD granularity from being changed.
  * Tightened the restrictions on the maximum SD that can be entered.
  * Moved the scale limit fields to the top of the list.
  * Fixed a small bug that sometimes showed more ticks than necessary on the X-axis.
  * Allow fixed values to be outside the scale range.

### 2018-05-22 13:32Z 0.12
  * Fixed a bug that caused a failure to calculate the possible SD range in some extreme samples.

### 2018-05-26 19:27Z 0.13
 * Added note about privatcy to the help text.
 * Added blank line before download link.
 * Added "loading" spinner image.
