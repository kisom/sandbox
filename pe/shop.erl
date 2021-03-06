-module(shop).
-export([cost/1, total/1]).

%% ShoppingList = [{oranges,4},{newspaper,1},{apples,10},{pears,6},{milk,3}]
cost (oranges) -> 5;
cost (newspaper) -> 8;
cost (apples) -> 2;
cost (pears) -> 9;
cost (milk) -> 7.

total(ShoppingList) ->
    lists:sum(
      lists:map(fun ({Item, Count}) ->
			Count * cost(Item) end,
		ShoppingList)).
