
(* Returns true if date x comes before date y chronologically
   false otherwise *)
fun is_older(x: int * int * int, y: int * int * int) =
    if (#1 x) <> (#1 y)
    then (#1 x) < (#1 y)
    else 
        if (#2 x) <> (#2 y)
        then (#2 x) < (#2 y)
        else (#3 x) < (#3 y)
    
(* Returns number of dates from a given list which fall in a given month *)
(* Returns 0 if none of the dates fall in the given month *)
fun number_in_month(xs: (int * int * int) list, month: int) =
    if null xs
    then 0
    else 
        (if #2 (hd xs) = month then 1 else 0) + number_in_month(tl xs, month)

(* Returns number of dates from the given list which fall in any of the
   months of a given list of months. If none of the dates fall in any of
   the months, then returns 0. Assumes that months do no contain duplicates. *)
fun number_in_months(xs: (int * int * int) list, months: int list) =
    if null months
    then 0
    else
        number_in_month(xs, hd months) + number_in_months(xs, tl months)

(* Returns a list of dates taken from the given list xs, which fall
   in the given month. If none of the dates fall in the given month, 
   then returns empty list. *)
fun dates_in_month(xs: (int * int * int) list, month: int) =
    if null xs
    then []
    else 
        let val tl_result = dates_in_month(tl xs, month)
        in
            if #2 (hd xs) = month then (hd xs) :: tl_result else tl_result    
        end

(* Returns a list of dates taken from the given list xs which fall
   in any of the given list of months. If none of the dates fall in
   any of the given months then returns empty list. *)
fun dates_in_months(xs: (int * int * int) list, months: int list) = 
    if null months
    then []
    else
        dates_in_month(xs, hd months) @ dates_in_months(xs, tl months)

(* Returns nth element of the list where head is the 1st element.
   Throws exception if xs does not have at least n elements *)
fun get_nth(xs: string list, n: int) =
    if n = 1 
    then hd xs
    else get_nth(tl xs, n - 1)

(* Converts the given date into a string of the format <Month Day, Year> *)
fun date_to_string(d: int * int * int) =
    let
        val months: string list = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in 
        get_nth(months, (#2 d)) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
    end

(* Returns a number n such that
   sum of first n elements of xs is less than sum and
   sum of first n + 1 elements of xs is at least sum.
   If the first element of the list itself is greater than sum, then
   this returns 0. However, if sum of entire list is less than sum, then
   it raises runtime exception. *)
fun number_before_reaching_sum(sum: int, xs: int list) =
    let
        fun sum_helper(ys: int list, acc: int, acc_index: int) = 
	    if acc < sum andalso acc + (hd ys) >= sum
	    then acc_index
	    else sum_helper(tl ys, acc + (hd ys), acc_index + 1)
    in
        sum_helper(xs, 0, 0)
    end

(* Returns the month in which a given day of year falls. Raises 
   exception if day is not between 1 and 365 inclusive. *)
fun what_month(day: int) =
    let
        val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 
    in
        number_before_reaching_sum(day, days_in_month) + 1
    end

(* Returns a list of months for all days between day1 and day2 inclusive.
   Returns an empty list if day1 > day2 *)
fun month_range(day1: int, day2: int) = 
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates: (int * int * int) list) =
    if null dates
    then NONE
    else 
        let 
            fun oldest_nonempty(xs: (int * int * int) list) = 
                if null (tl xs)
                then hd xs
                else
                    let val oldest_in_tail = oldest_nonempty(tl xs)
                    in 
                        if is_older(hd xs, oldest_in_tail) 
                        then hd xs
                        else oldest_in_tail
                    end
        in
            SOME(oldest_nonempty dates)
        end

fun contains(ys: int list, y: int) = 
    if null ys
    then false
    else y = (hd ys) orelse contains(tl ys, y)

fun removeDuplicates(ys: int list, acc: int list) =
    if null ys
    then acc
    else 
	if contains(acc, hd ys) 
	then removeDuplicates(tl ys, acc)
	else removeDuplicates(tl ys, acc @ [(hd ys)])


(* Returns number of dates from the given list which fall in any of the
   months of a given list of months. If none of the dates fall in any of
   the months, then returns 0. Works even if months list has duplicates *)
fun number_in_months_challenge(xs: (int * int * int) list, months: int list) =
    number_in_months(xs, removeDuplicates(months, []))

fun dates_in_months_challenge(xs: (int * int * int) list, months: int list) = 
    dates_in_months(xs, removeDuplicates(months, []))

fun reasonable_date(date: int * int * int) = 
    let
        val days_in_month_normal = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 
        val days_in_month_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 
        val is_leap_year = (#1 date) mod 400 = 0 orelse ((#1 date) mod 4 = 0 andalso (#1 date) mod 100 > 0)
        fun get_nth(xs: int list, n: int) =
	    if n = 1 
	    then hd xs
	    else get_nth(tl xs, n - 1)

    in
	if (#1 date) <= 0
	then false
	else
	    if ((#2 date) < 1 orelse (#2 date) > 12)
	    then false
	    else 
                let val max_days = if is_leap_year  
                                   then get_nth(days_in_month_leap, (#2 date))
                                   else get_nth(days_in_month_normal, (#2 date))
                in
                    (#3 date) >=1 andalso (#3 date) <= max_days
                end
    end
