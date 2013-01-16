val is_older_test1 = is_older((1985,11,16),(1985,11,16)) = false
val is_older_test2 = is_older((1985,11,16),(1985,11,15)) = false
val is_older_test3 = is_older((1985,11,16),(1985,11,17)) = true
val is_older_test4 = is_older((1985,11,16),(1985,10,16)) = false
val is_older_test5 = is_older((1985,11,16),(1985,12,16)) = true
val is_older_test6 = is_older((1985,11,16),(1984,11,16)) = false
val is_older_test7 = is_older((1985,11,16),(1986,11,16)) = true

val number_in_month_test1 = number_in_month([], 3) = 0
val number_in_month_test2 = number_in_month([(1985,11,16)], 3) = 0
val number_in_month_test3 = number_in_month([(1985,11,16),(1988,7,11)], 3) = 0
val number_in_month_test4 = number_in_month([(1985,11,16),(1988,3,17),(1988,7,29)], 3) = 1
val number_in_month_test5 = number_in_month([(1985,11,16),(1977,3,4),(1988,7,29),(19,3,1)], 3) = 2

val number_in_months_test1 = number_in_months([], []) = 0
val number_in_months_test2 = number_in_months([], [3,4,5]) = 0
val number_in_months_test3 = number_in_months([(1985,11,16),(1988,7,29)], []) = 0
val number_in_months_test4 = number_in_months([(1985,11,16),(1977,3,4),(1988,7,29),(19,3,1)], [3]) = 2
val number_in_months_test5 = number_in_months([(1985,11,16),(1977,3,4),(1988,7,29),(19,3,1)], [3,11,7]) = 4

val dates_in_month_test1 = dates_in_month([], 3) = []
val dates_in_month_test2 = dates_in_month([(1985,11,16)], 1) = []
val dates_in_month_test3 = dates_in_month([(1985,11,16)], 11) = [(1985,11,16)]
val dates_in_month_test4 = dates_in_month([(1985,11,16),(1977,3,4),(1988,7,29),(19,3,1)], 3) = [(1977,3,4),(19,3,1)]

val dates_in_months_test1 = dates_in_months([], []) = []
val dates_in_months_test2 = dates_in_months([], [1,2]) = []
val dates_in_months_test3 = dates_in_months([(1985,11,16),(1977,3,4)], []) = []
val dates_in_months_test4 = dates_in_months([(1985,11,16),(1977,3,4)], [7,8]) = []
val dates_in_months_test5 = dates_in_months([(1985,11,16),(1977,3,4),(293847,4,23)], [3,4]) = [(1977,3,4),(293847,4,23)]

val get_nth_test1 = get_nth(["hi"], 1) = "hi"
val get_nth_test2 = get_nth(["hi", "ho", "ha", "hu"], 1) = "hi"
val get_nth_test3 = get_nth(["hi", "ho", "ha", "hu"], 2) = "ho"
val get_nth_test4 = get_nth(["hi", "ho", "ha", "hu"], 3) = "ha"
val get_nth_test5 = get_nth(["hi", "ho", "ha", "hu"], 4) = "hu"

val date_to_string_test1 = date_to_string((1,1,1)) = "January 1, 1"
val date_to_string_test2 = date_to_string((1985,11,16)) = "November 16, 1985"

val number_before_reaching_sum_test1 = number_before_reaching_sum(0, [1,2,3,4,5]) = 0
val number_before_reaching_sum_test2 = number_before_reaching_sum(10, [1,2,3,4,5,6,7]) = 3
val number_before_reaching_sum_test3 = number_before_reaching_sum(11, [1,2,3,4,5,6,7]) = 4

val what_month_test1 = what_month(1) = 1
val what_month_test2 = what_month(31) = 1
val what_month_test3 = what_month(32) = 2
val what_month_test4 = what_month(59) = 2
val what_month_test5 = what_month(60) = 3
val what_month_test6 = what_month(365) = 12

val month_range_test1 = month_range(3,1) = []
val month_range_test2 = month_range(3,3) = [1]
val month_range_test3 = month_range(31,33) = [1,2,2]

val oldest_test1 = oldest([]) = NONE
val oldest_test2 = oldest([(1985,11,16)]) = SOME((1985,11,16))
val oldest_test3 = oldest([(1985,11,16),(1955,11,17),(1912,10,17),(1989,1,12)]) = SOME((1912,10,17))
