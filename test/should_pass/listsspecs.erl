-module(listsspecs).

-compile([export_all, nowarn_export_all]).

-spec append([integer()]) -> [integer()].
append(X) ->
    lists:append([1,2,3],X).

-spec delete() -> [integer()].
delete() ->
    lists:delete(1,[1,2,3]).

-spec droplast() -> [integer()].
droplast() ->
    lists:droplast([1]).

-spec dropwhile() -> [integer()].
dropwhile() ->
    lists:dropwhile(fun (X) -> X rem 2 == 0 end, [2,3,4]).

-spec duplicate() -> [boolean()].
duplicate() ->
    lists:duplicate(5, true).

-spec filter() -> [integer()].
filter() ->
    lists:filter(fun (X) -> X rem 2 == 0 end, [2,3,4]).

-spec filtermap() -> [integer()].
filtermap() ->
    lists:filter(fun (X) -> X rem 2 == 0 end, [2,3,4]).

-spec flatmap() -> string().
flatmap() ->
    lists:flatmap(fun (X) -> integer_to_list(X) end, [1,2,3]).

-spec flatten() -> [integer()].
flatten() ->
    lists:flatten([[1,2],[3,4]]).

-spec foldl() -> integer().
foldl() ->
    lists:foldl(fun (A, B) -> A + B end, 0, [1, 2, 3]).

-spec foldr() -> integer().
foldr() ->
    lists:foldr(fun (A, B) -> A + B end, 0, [1, 2, 3]).

-spec join() -> [string()].
join() ->
    lists:join("/", ["a", "b", "c"]).

-spec foreach() -> ok.
foreach() ->
    lists:foreach(fun (X) -> X+1 end, [1, 2, 3]).

-spec last() -> 1.
last() ->
    lists:last([1]).

-spec map() -> [integer()].
map() ->
    lists:map(fun (X) -> X+1 end, [1, 2, 3]).

-spec mapfoldl() -> {[integer()], integer()}.
mapfoldl() ->
    lists:mapfoldl(fun (X, Acc) -> {Acc, X + Acc} end, 0, [1, 2, 3]).

-spec mapfoldr() -> {[integer()], integer()}.
mapfoldr() ->
    lists:mapfoldr(fun (X, Acc) -> {Acc, X + Acc} end, 0, [1, 2, 3]).

-spec max() -> integer().
max() ->
    lists:max([1, 2, 3]).

-spec min() -> integer().
min() ->
    lists:min([1, 2, 3]).

-spec merge() -> [integer()].
merge() ->
    lists:merge([[1, 2], [3]]).

-spec merge_() -> [integer()].
merge_() ->
    lists:merge([1, 2], [3]).

-spec merge__() -> [integer() | boolean()].
merge__() ->
    lists:merge(fun (_,_) -> true end, [1, 2, 3], [false, true]).

-spec merge3() -> [integer()].
merge3() ->
    lists:merge3([1, 2, 3], [4, 5, 6], [7, 8, 9]).

-spec nth() -> integer().
nth() ->
    lists:nth(5,[1, 2, 3, 4, 5, 6]).

-spec nthtail() -> [integer()].
nthtail() ->
    lists:nthtail(5,[1, 2, 3, 4, 5, 6]).

-spec partition() -> {[integer()], [integer()]}.
partition() ->
    lists:partition(fun (X) -> X > 3 end, [1, 2, 3, 4, 5]).

-spec reverse() -> [integer()].
reverse() ->
    lists:reverse([1, 2, 3]).

-spec reverse_() -> [integer()].
reverse_() ->
    lists:reverse([1, 2, 3], [4, 5, 6]).

-spec sort() -> [integer()].
sort() ->
    lists:sort([1, 2, 3]).

-spec sort_() -> [integer()].
sort_() ->
    lists:sort(fun (_,_) -> true end, [1, 2, 3]).

-spec split() -> {[integer()], [integer()]}.
split() ->
    lists:split(3, [1, 2, 3, 4, 5]).

-spec splitwith() -> {[integer()], [integer()]}.
splitwith() ->
    lists:splitwith(fun (X) -> X > 3 end, [1, 2, 3, 4, 5]).

-spec sublist() -> [integer()].
sublist() ->
    lists:sublist([1, 2, 3, 4], 2).

-spec sublist_() -> [integer()].
sublist_() ->
    lists:sublist([1, 2, 3, 4], 1, 2).

-spec subtract() -> [integer()].
subtract() ->
    lists:subtract([1, 2, 3, 4, 5], [2, 3]).

-spec takewhile() -> [integer()].
takewhile() ->
    lists:takewhile(fun (X) -> X < 3 end, [1, 2, 3, 4]).

-spec umerge() -> [integer()].
umerge() ->
    lists:umerge([[1, 2, 3], [1, 2, 3, 4]]).

-spec umerge_() -> [integer()].
umerge_() ->
    lists:umerge([1, 2, 3], [1, 2, 3, 4]).

-spec umerge__() -> [integer()].
umerge__() ->
    lists:umerge(fun (X, Y) -> X < Y end, [1, 2, 3], [1, 2, 3, 4]).

-spec umerge3() -> [integer()].
umerge3() ->
    lists:umerge3([1, 2, 3], [1, 2, 3, 4], [2, 3]).

-spec unzip() -> {[integer()], [boolean()]}.
unzip() ->
    lists:unzip([{1, true}, {2, false}]).

-spec unzip3() -> {[integer()], [boolean()], [string()]}.
unzip3() ->
    lists:unzip3([{1, true, "apa"}, {2, false, "bepa"}]).

-spec usort() -> [integer()].
usort() ->
    lists:usort([1, 2, 3]).

-spec usort_() -> [integer()].
usort_() ->
    lists:usort(fun (_,_) -> true end, [1, 2, 3]).

-spec zip() -> [{integer(), atom()}].
zip() ->
    lists:zip([1, 2, 3], [a, b, c]).

-spec zip3() -> [{boolean(), integer(), atom()}].
zip3() ->
    lists:zip3([true, false, true], [1, 2, 3], [a, b, c]).

-spec zipwith() -> [integer()].
zipwith() ->
    lists:zipwith(fun (X, _) -> X end, [1, 2, 3], [a, b, c]).

-spec zipwith3() -> [integer()].
zipwith3() ->
    lists:zipwith3(fun (_, X, _) -> X end,
		   [true, false, true], [1, 2, 3], [a, b, c]).
