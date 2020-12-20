# Ecology-Calculations
R functions related to analyzing silphium populations

1) Make_life_table generates a seperate lifetable for every geographic coordinate with output as a single dataframe.
2) Create_lx_curve generates an ln(lx) survivorship curve for every geographic coordinate plotted into one graph.
3) lx_year is the same as 2. but also includes a seperate plot for every year incorperated into the input data. Requires a new column with containing "year". 
4) parasite_lx generates a ln(lx) survivorship curve for every geographic coordinate but colors each line according to parasitism status.
5) Create_vx_curve generates and graphs multiple lines of vx v gx grouped by geographic coordinates.
6) Ro_CI_by_parasitism creates a 95% CI error bar diagram for any locality comparing the mean Ro for all parasitized and not-parasisitized geographic subpopulations in a single locality.
7) Ro_CI_by_year creates a 95% CI error bar diagram for any locality comparing the mean Ro for every subpopulation at a single location colored by as many years as are in the input data. Requires a new column with containing "year". 
