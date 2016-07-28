fun is_older (date1 : int * int * int, date2 : int * int * int) =
    if #1 date1 < #1 date2
    then true
    else if (#1 date1 <= #1 date2) andalso (#2 date1 < #2 date2)
    then true
    else
    (#1 date1 <= #1 date2) andalso
    (#2 date1 <= #2 date2) andalso
    (#3 date1 < #3 date2)

fun number_in_month (dates : (int * int * int) list, mon : int) =
    if null(dates)
    then 0
    else if mon = #2 (hd(dates))
    then 1 + number_in_month(tl(dates), mon)
    else number_in_month(tl(dates), mon)

fun number_in_months (dates : (int * int * int) list, mons : int list) =
    if null(mons)
    then 0
    else number_in_month(dates, hd(mons)) + number_in_months(dates, tl(mons))

fun dates_in_month (dates : (int * int * int) list, mon : int) =
    if null(dates)
    then []
    else if mon = #2 (hd dates)
    then (hd dates) :: dates_in_month(tl(dates), mon)
    else dates_in_month(tl(dates), mon)

fun dates_in_months (dates : (int * int * int) list, mons : int list) =
    if null(mons)
    then []
    else dates_in_month(dates, hd(mons)) @ dates_in_months(dates, tl(mons))

fun get_nth (s : string list, i : int) =
    if i = 1
    then hd(s)
    else get_nth(tl(s), i - 1)

fun date_to_string (date : int * int * int) =
    let val months = ["January", "February", "March", "April",
              "May", "June", "July", "August", "September",
              "October", "November", "December"]
    in
    get_nth(months, #2 date) ^ " " ^
    Int.toString(#3 date) ^ ", " ^
    Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum : int, lst : int list) =
    let fun sum_helper(ilst : int list, list_sum : int, index : int) =
    if list_sum >= sum
    then index
    else sum_helper(tl(ilst), list_sum + hd(ilst), index + 1)
    in
    sum_helper(lst, 0, ~1)
    end

(*

fun number_before_reaching_sum (sum : int, lst : int list) =
    if hd(lst) >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - hd(lst), tl(lst))

*)

fun what_month (day : int) =
    let val days_of_month = [31, 28, 31, 30, 31, 30,
                31, 31, 30, 31, 30, 31]
    in
    1 + number_before_reaching_sum(day, days_of_month)
    end

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (dates : (int * int * int) list) =
    if null(dates)
    then NONE
    else
    let
        val tl_ans = oldest(tl(dates))
    in
        if isSome(tl_ans) andalso is_older(valOf(tl_ans), hd(dates))
        then tl_ans
        else SOME (hd(dates))
    end
