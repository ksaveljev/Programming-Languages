(*
 * In order to compare dates we need to compare year, month and day
 * in that particular order. Only if the entity we are comparing is
 * the same as in the second date we proceed to the next entity,
 * otherwise we can instantly make a conclusion based on the
 * comparison of the entity
 *)
fun is_older(a: int*int*int, b: int*int*int) =
  if #1 a = #1 b
  then
    if #2 a = #2 b then #3 a < #3 b
    else #2 a < #2 b
  else
    #1 a < #1 b

(*
 * Recursively go through the list comparing each date's month
 * to the value we are interested in. If the month matches ours
 * we add 1 to the result, if it doesn't then nothing is added
 *)
fun number_in_month(dates: (int*int*int) list, month: int) =
  if null dates then 0
  else
    if #2 (hd dates) = month then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

(*
 * Problem statement tells us to assume "months" has no duplicates
 *
 * Recursively go through the list of months reusing the function
 * we already implemented (number_in_month)
 *)
fun number_in_months(dates: (int*int*int) list, months: int list) =
  if null months then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(*
 * Recursively go through the list comparing each date's month
 * to the value we are interested in. If the month matches ours
 * we add the date to the result, if it doesn't then nothing is added
 *)
fun dates_in_month(dates: (int*int*int) list, month: int) =
  if null dates then []
  else
    if #2 (hd dates) = month then hd dates :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

(*
 * Problem statement tells us to assume "months" has no duplicates
 *
 * Recursively go through the list of months reusing the function
 * we already implemented (dates_in_month)
 *)
fun dates_in_months(dates: (int*int*int) list, months: int list) =
  if null months then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(*
 * Recursively subtract head from strings list and 1 from n until
 * n is equal to one. At this point we are looking at the element
 * we are interested in (head of the strings list)
 *)
fun get_nth(strings: string list, n: int) =
  if n = 1 then hd strings
  else get_nth(tl strings, n-1)

(*
 * Reuse the function we implemented before (get_nth) to get the
 * name of the month we are looking for and simply use string
 * concatenation to get the needed result
 *)
fun date_to_string(date: int*int*int) =
  let
    val months = ["January", "February", "March", "April", "May", "June", "July",
    "August", "September", "October", "November", "December"]
  in
    get_nth(months, #2 date)
    ^ " " ^ Int.toString(#3 date)
    ^ ", " ^ Int.toString(#1 date)
  end

(*
 * Using a helper function we can recursively go down the numbers list
 * keeping track of the sum of the elements we visited (in accumulator variable)
 * and return from it as soon as the sum reaches the limit
 *)
fun number_before_reaching_sum(sum: int, numbers: int list) =
  let
    fun count(numbers: int list, acc: int, n: int) =
      if hd numbers + acc >= sum then n
      else count(tl numbers, acc + hd numbers, n + 1)
  in
    count(numbers, 0, 0)
  end

(*
 * Reuse the function we have already implemented (number_before_reaching_sum)
 * to easily find position of the month we are interested in. Becuase the
 * function number_before_reaching_sum stops right before the element which
 * is going to exceed the sum we need to add 1 to final result for this
 * function to get the month the days falls into (so we want to exceed the sum)
 *)
fun what_month(day: int) =
  let
    val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, days_in_month) + 1
  end

(*
 * Recursively build the answer reusing the function we already implemented
 * (what_month)
 *)
fun month_range(day1: int, day2: int) =
  if day1 > day2 then []
  else what_month(day1) :: month_range(day1 + 1, day2)

(*
 * Recursively go through the list of dates and compare the dates
 *)
fun oldest(dates: (int*int*int) list) =
  if null dates then NONE
  else 
    let
      val tl_ans = oldest(tl dates)
    in
      if isSome tl_ans andalso is_older(valOf tl_ans, hd dates) then tl_ans
      else SOME (hd dates)
    end
    
(*
 * CHALLENGE PROBLEMS
 *
 * no explanations for these tasks
 *)

(* helper function *)
fun delete(e: int, elements: int list) =
  if null elements then []
  else
    if e = hd elements then delete(e, tl elements)
    else hd elements :: delete(e, tl elements)

(* helper function *)
fun removeDuplicates(months: int list) =
  if null months then []
  else hd months :: removeDuplicates(delete(hd months, tl months))

fun number_in_months_challenge(dates: (int*int*int) list, months: int list) =
  number_in_months(dates, removeDuplicates(months))

fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) =
  dates_in_months(dates, removeDuplicates(months))

fun reasonable_date(date: int*int*int) =
  let
    val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    val days_in_month_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

    fun is_leap(year: int) =
      year mod 400 = 0 orelse year mod 4 = 0 andalso year mod 100 <> 0

    fun get_nth(ints: int list, n: int) =
      if n = 1 then hd ints
      else get_nth(tl ints, n - 1)

    fun is_valid_year(year: int) = year > 0
    fun is_valid_month(month: int) = month >= 1 andalso month <= 12
    fun is_valid_day(day: int) = day >= 1 andalso day <= 31

    fun is_valid_date() =
      if is_leap(#1 date) then (#3 date) <= get_nth(days_in_month_leap, #2 date)
      else (#3 date) <= get_nth(days_in_month, #2 date)
  in
    is_valid_year(#1 date) andalso is_valid_month(#2 date)
    andalso is_valid_day(#3 date) andalso is_valid_date()
  end
